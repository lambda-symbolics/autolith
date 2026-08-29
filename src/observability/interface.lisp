(in-package #:autolith)

;;;; -- Observability Facade --

(-> observability-configure (&rest t) nemo-relay-configuration)
(defun observability-configure (&rest arguments)
  "Configure the active observability backend through its private adapter."
  (apply #'nemo-relay-configure arguments))

(-> observability-enabled-p () boolean)
(defun observability-enabled-p ()
  "Return true when observability is enabled by explicit or environment settings."
  (nemo-relay-enabled-p))

(-> observability-last-error () (option string))
(defun observability-last-error ()
  "Return the latest non-fatal observability diagnostic."
  (nemo-relay-last-error))

(-> observability-output-pathname () (option pathname))
(defun observability-output-pathname ()
  "Return the first configured local observability output pathname, when known."
  (nemo-relay-output-pathname))

(-> observability-flush () boolean)
(defun observability-flush ()
  "Wait for queued observability callbacks to complete."
  (nemo-relay-flush))

(-> observability-start (&optional (option configuration)) boolean)
(defun observability-start (&optional configuration)
  "Start observability for CONFIGURATION without affecting core execution."
  (nemo-relay-start configuration))

(-> observability-stop () null)
(defun observability-stop ()
  "Flush and stop observability idempotently."
  (nemo-relay-shutdown))

(-> observability-prepare-checkpoint () null)
(defun observability-prepare-checkpoint ()
  "Drop process-local observability state before saving a checkpoint image."
  (nemo-relay--detach-for-checkpoint))


;;;; -- Context Propagation --

(-> capture-observability-context () (option t))
(defun capture-observability-context ()
  "Capture the current opaque observability context for another execution boundary."
  (nemo-relay-current-propagation-context))

(-> observability--call-with-context (t function) t)
(defun observability--call-with-context (context function)
  "Run FUNCTION with opaque observability CONTEXT installed."
  (let ((*nemo-relay-propagation-context-json* context))
    (funcall function)))

(defmacro with-observability-context (context &body body)
  "Run BODY with CONTEXT installed at an observability execution boundary."
  `(observability--call-with-context
    ,context
    (lambda () ,@body)))


;;;; -- Semantic Marks --

(-> observability-mark (keyword &rest t) null)
(defun observability-mark (event &rest properties)
  "Record the supported semantic EVENT without exposing backend payloads to core."
  (case event
    (:goal-completed
     (nemo-relay-mark
      "autolith.goal.completed"
      :data (json-object "objective" (getf properties :objective))))
    (:checkpoint-created
     (let ((generation (getf properties :generation)))
       (nemo-relay-mark
        "autolith.checkpoint.created"
        :data
        (json-object
         "generation" (generation-identifier generation)
         "coordinator_pid" (generation-coordinator-pid generation)))))
    (:compaction-started
     (nemo-relay-mark
      "autolith.compaction.started"
      :data (json-object "total_tokens" (getf properties :total-tokens))))
    (:compaction-completed
     (nemo-relay-mark
      "autolith.compaction.completed"
      :data
      (json-object
       "summary_characters" (getf properties :summary-characters)
       "native" (if (getf properties :native-p) t false))))
    ((:tool-authorization-requested :tool-authorization-denied)
     (let* ((tool (getf properties :tool))
            (tool-name
              (and tool
                   (if (stringp tool)
                       tool
                       (tool-canonical-name tool))))
            (name (if (eq event ':tool-authorization-requested)
                      "autolith.tool.authorization.requested"
                      "autolith.tool.authorization.denied")))
       (nemo-relay-mark
        name
        :data
        (json-object
         "kind" (getf properties :kind)
         "tool" tool-name))))
    (otherwise
     nil))
  nil)

(-> observability-mark-provider-retry (integer integer real) null)
(defun observability-mark-provider-retry (attempt maximum-attempts delay)
  "Record one provider retry without constructing backend data in core."
  (nemo-relay-mark
   "autolith.provider.retry"
   :data
   (json-object
    "attempt" attempt
    "maximum_attempts" maximum-attempts
    "delay_seconds" delay))
  nil)


;;;; -- Lifecycle Boundaries --

(defgeneric observability-agent-turn-metadata (agent &key automatic-p)
  (:documentation "Return backend-neutral agent-turn metadata for AGENT."))

(defmethod observability-agent-turn-metadata ((agent t) &key automatic-p)
  "Return minimal metadata for an agent without a specialized adapter."
  (declare (ignore agent))
  (json-object
   "agent" "autolith"
   "child" false
   "automatic" (if automatic-p t false)))

(defgeneric observability-agent-turn-start-data (agent)
  (:documentation "Return optional backend-neutral start data for AGENT's turn."))

(defmethod observability-agent-turn-start-data ((agent t))
  "Return no start data for an agent without a specialized adapter."
  (declare (ignore agent))
  nil)

(-> observability--call-with-agent-turn
    (t t &key (:automatic-p boolean) (:function function))
    t)
(defun observability--call-with-agent-turn
    (agent content &key automatic-p function)
  "Run FUNCTION inside the backend's agent-turn lifecycle boundary."
  (if (not (observability--active-p))
      (funcall function)
      (nemo-relay--call-with-agent
       :name "autolith.turn"
       :input (nemo-relay--agent-input content)
       :metadata
       (observability-agent-turn-metadata agent :automatic-p automatic-p)
       :function
       (lambda ()
         (let ((data (observability-agent-turn-start-data agent)))
           (when data
             (nemo-relay-mark "autolith.turn.started" :data data)))
         (funcall function)))))

(defmacro with-observed-agent-turn ((agent content &key automatic-p) &body body)
  "Run BODY inside the optional observability boundary for one agent turn."
  `(if (observability--active-p)
       (observability--call-with-agent-turn
        ,agent ,content
        :automatic-p ,automatic-p
        :function (lambda () ,@body))
       (progn ,@body)))


(-> observability--call-with-tool
    (json-object &key (:function function))
    t)
(defun observability--call-with-tool (call &key function)
  "Run FUNCTION inside the optional observability boundary for one tool CALL."
  (if (not (observability--active-p))
      (funcall function)
      (nemo-relay--call-with-tool
       :name (function-call-canonical-name call)
       :call-id (json-get call "call_id")
       :arguments (or (json-get call "arguments") "{}")
       :function function)))

(defmacro with-observed-tool-call ((call) &body body)
  "Run BODY inside the optional observability boundary for one tool call."
  `(if (observability--active-p)
       (observability--call-with-tool
        ,call
        :function (lambda () ,@body))
       (progn ,@body)))
