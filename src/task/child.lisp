(in-package #:autolith)

;;;; -- Task Child Execution --

(-> task-conversation-depth (configuration conversation) (integer 0))
(defun task-conversation-depth (configuration conversation)
  "Count registered channel ancestors and explicit unregistered parent links."
  (if (conversation-inbox-p conversation)
      0
      (let* ((world   (world-load configuration))
             (channel (world-channel-find world (conversation-identifier conversation)))
             (seen    (make-hash-table :test #'equal)))
        (loop for parent = (if channel
                               (channel-parent channel)
                               (conversation-parent-identifier conversation)) then (and
                                                                                    channel
                                                                                    (channel-parent
                                                                                     channel))
              while (and parent (not (gethash parent seen))
                         (let ((ancestor (world-channel-find world parent)))
                           (not (and ancestor (channel-inbox-p ancestor)))))
              count t into depth
              do (setf (gethash parent seen) t
                       channel (world-channel-find world parent))
              finally (return (max depth 0))))))

(-> task-child-tool-registry
    (tool-registry &key (:orchestrator (option task-orchestrator)))
    tool-registry)
(defun task-child-tool-registry (parent-registry &key orchestrator)
  "Copy the configured tools and borrow the shared runtimes of PARENT-REGISTRY."
  (let* ((registry
          (make-instance 'tool-registry :resource-registry
                         (tool-registry-resource-registry parent-registry)))
         (manager (mcp-tool-registry-manager parent-registry)))
   (maphash
    (lambda (key binding) (unless (eq key :mcp) (tool-registry-bind-runtime registry key binding)))
    (tool-registry-runtime-bindings parent-registry))
   (tool-registry-bind-runtime registry 'task-orchestrator orchestrator)
   (maphash
    (lambda (namespace description)
      (tool-registry-describe-namespace registry namespace description))
    (tool-registry-namespace-descriptions parent-registry))
   (dolist (tool (tool-registry-tools parent-registry))
     (unless (typep tool 'mcp-provider-tool) (tool-registry-register registry tool)))
   (when manager
     (mcp-tool-registry-bind-manager registry manager)
     (mcp-tool-registry-refresh registry :only-dirty-p t)
     (mcp-tool-registry-activate-discovered-tools registry))
   registry))

(defparameter *task-default-reasoning-effort*
  "medium"
  "Default reasoning effort for delegated child assignments.")

(-> task-child-configuration (configuration list) configuration)
(defun task-child-configuration (parent-configuration item)
  "Copy PARENT-CONFIGURATION with the model and effort selected by ITEM."
  (let ((model  (or (find-if (lambda (candidate)
                               (member candidate *supported-models* :test #'string=))
                             (getf item :models))
                    (config :model parent-configuration)))
        (effort (or (getf item :reasoning-effort) *task-default-reasoning-effort*)))

    (configuration-copy parent-configuration
                        :model model :reasoning-effort effort :codex-fast-mode-p nil)))

(-> task-child-reference-history-p (agent configuration) boolean)
(defun task-child-reference-history-p (parent child-configuration)
  "Return true when PARENT and CHILD-CONFIGURATION's provider both opt in."
  (and (provider-child-reference-history-p (agent-provider parent))
       (provider-child-reference-history-p
        (provider-with-configuration
         (agent-provider parent) child-configuration))))

(-> task-child-reference-history-byte-limit
    (configuration)
    (option integer))
(defun task-child-reference-history-byte-limit (configuration)
  "Return a conservative inherited-reference wire budget, or NIL when too small.

UTF-8 bytes are an upper bound on tokenizer tokens, so this leaves at least
three quarters of the child context for instructions, tools, and new work while
also applying a fixed retention cap. Inheritance is disabled when its mandatory
boundary cannot fit within that budget."
  (let ((limit
          (min *task-inherited-reference-maximum-bytes*
               (max 1
                    (floor (config :context-window configuration)
                           *task-inherited-reference-context-divisor*)))))
    (and (>= limit
             (conversation--inherited-reference-wire-byte-length nil))
         limit)))

(-> task--bounded-output ((option string)) string)
(defun task--bounded-output (text)
  "Bound TEXT by configured UTF-8 bytes and lines, marking truncation."
  (let* ((maximum-bytes
          (task--environment-integer "AUTOLITH_TASK_MAX_OUTPUT_BYTES"
                                     *task-default-maximum-output-bytes*
                                     :minimum 1))
         (maximum-lines
          (task--environment-integer "AUTOLITH_TASK_MAX_OUTPUT_LINES"
                                     *task-default-maximum-output-lines*
                                     :minimum 1))
         (lines          (task--split-lines (or text "")))
         (line-bounded-p (> (length lines) maximum-lines))
         (line-text
          (format nil "~{~A~^~%~}"
                  (subseq lines 0 (min maximum-lines (length lines)))))
         (prefix-end     (utf8-string-prefix-end line-text maximum-bytes))
         (byte-bounded-p (< prefix-end (length line-text)))
         (bounded        (if byte-bounded-p
                             (subseq line-text 0 prefix-end)
                             line-text)))
    (if (or line-bounded-p byte-bounded-p)
        (format nil "~A~%... [task output truncated]" bounded)
        bounded)))

(defparameter *task-result-maximum-octets* (* 16 1024 1024)
  "The largest durable child result accepted by task inspection.")

(-> task--result-grammar () source-grammar)
(defun task--result-grammar ()
  "Return the bounded portable data dialect used by durable child results."
  (make-source-grammar
   :label                                     "Child result artifact"
   :maximum-depth 128 :maximum-nodes 262144
   :maximum-string-characters                 *task-result-maximum-octets*
   :qualified-common-lisp-symbols-permitted-p t
   :readable-strings-permitted-p              t
   :read-default-float-format                 'single-float
   :allowed-atom-predicate
   (lambda (value)
     (or (null value) (eq value t) (keywordp value) (stringp value)
         (cl-llm-provider-api:output-json-number-p value)))))

(-> task--result-fields () list)
(defun task--result-fields ()
  "Describe the terminal child result fields consumed by job inspection."
  (append
   (list (list :indicator ':status :required t
               :validate (lambda (value)
                           (not (null (member value '(:success :failed :aborted))))))
         (list :indicator ':request-count
               :validate  (lambda (value) (typep value '(integer 0))))
         (list :indicator ':duration-ms
               :validate  (lambda (value) (typep value '(integer 0))))
         (list :indicator ':usage
               :validate  (lambda (value)
                            (or (null value) (task-progress--usage-object-p value)))))
   (mapcar (lambda (key)
             (list :indicator key
                   :validate  (lambda (value) (or (null value) (stringp value)))))
           '(:id :name :assignment :error :label :model :conversation-file))
   (mapcar (lambda (key)
             (list :indicator key :validate (lambda (value) (typep value 'boolean))))
           '(:detached))))

(-> task--read-result-artifact
    ((or pathname string) &key (:maximum-octets (integer 1))) list)
(defun task--read-result-artifact
    (pathname &key (maximum-octets *task-result-maximum-octets*))
  "Read exactly one bounded, schema-checked durable child result."
  (sexp-store:snapshot-read-record
   pathname :grammar (task--result-grammar) :maximum-octets maximum-octets
   :properties-p t :keyword-keys-p t :maximum-length 128
   :fields (task--result-fields)))
