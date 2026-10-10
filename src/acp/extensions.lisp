(in-package #:autolith)

;;;; -- Typed ACP Extensions --

(define-condition acp-extension-unavailable (error)
  ((reason :initarg :reason :reader acp-extension-unavailable-reason
           :documentation "Why the requested headless operation is unavailable."))
  (:report (lambda (condition stream)
             (write-string (acp-extension-unavailable-reason condition) stream)))
  (:documentation "An extension operation has no admitted headless implementation."))

(defparameter *acp-extension-methods*
  '(("_autolith/state" acp-extension-state :read)
    ("_autolith/settings" acp-extension-settings :read)
    ("_autolith/models" acp-extension-models :read)
    ("_autolith/set-setting" acp-extension-set-setting :write)
    ("_autolith/operations" acp-extension-operations :read)
    ("_autolith/invoke" acp-extension-invoke :read)
    ("_autolith/conversations" acp-extension-conversations :read)
    ("_autolith/jobs" acp-extension-jobs :read)
    ("_autolith/job" acp-extension-job :read)
    ("_autolith/job-transcript" acp-extension-job-transcript :read)
    ("_autolith/job-send" acp-extension-job-send :read)
    ("_autolith/job-cancel" acp-extension-job-cancel :read)
    ("_autolith/resource" acp-extension-resource :read))
  "Implemented session methods, their handlers and primary-operation admission.")

(-> acp-extension-tool-context (acp-session) tool-context)
(defun acp-extension-tool-context (session)
  "Construct a session-owned context with ACP permission decisions, never terminal prompts."
  (let* ((application (acp-session-application session))
         (observer (acp-observer-create session)))
    (make-instance 'tool-context
                   :configuration (application-configuration application)
                   :worker (application-worker application)
                   :conversation (application-conversation application)
                   :registry (application-tool-registry application)
                   :agent (application-agent application)
                   :observer observer
                   :command-authorization-function
                   (lambda (command directory)
                     (agent-observer-authorize-command observer command directory))
                   :tool-authorization-function
                   (lambda (tool arguments)
                     (agent-observer-authorize-tool observer tool arguments)))))

(-> acp-extension-value (t) t)
(defun acp-extension-value (value)
  "Copy portable VALUE into bounded JSON data without printing opaque runtime objects."
  (let ((nodes 0))
    (labels ((convert (item depth)
               "Convert one value within the document's shared structural bounds."
               (when (or (> (incf nodes) 16384) (> depth 32))
                 (error 'acp-extension-unavailable :reason "The extension value exceeds its structural limit."))
               (cond
                 ((null item) ':null)
                 ((eq item t) t)
                 ((argo:json-false-p item) (argo:json-false))
                 ((stringp item)
                  (when (> (length item) 262144)
                    (error 'acp-extension-unavailable :reason "The extension string exceeds its character limit."))
                  (copy-seq item))
                 ((or (integerp item) (floatp item)) item)
                 ((pathnamep item) (namestring item))
                 ((keywordp item)
                  (if (eq item ':null) ':null (string-downcase (symbol-name item))))
                 ((hash-table-p item)
                  (let ((copy (make-hash-table :test #'equal)))
                    (maphash (lambda (key value)
                               (unless (stringp key)
                                 (error 'acp-extension-unavailable :reason "An extension object has a non-string key."))
                               (setf (gethash (copy-seq key) copy) (convert value (1+ depth))))
                             item)
                    copy))
                 ((consp item)
                  (let ((items nil))
                    (loop for tail = item then (rest tail)
                          while tail
                          do (unless (consp tail)
                               (error 'acp-extension-unavailable :reason "An extension list is not proper."))
                             (push (convert (first tail) (1+ depth)) items))
                    (coerce (nreverse items) 'vector)))
                 ((vectorp item)
                  (map 'vector (lambda (value) (convert value (1+ depth))) item))
                 (t
                  (error 'acp-extension-unavailable :reason "The value has no portable extension representation.")))))
      (convert value 0))))

(-> acp-extension-result (keyword &key (:value t) (:report (option string))
                                     (:condition-type (option string))) hash-table)
(defun acp-extension-result (outcome &key value report condition-type)
  "Construct one typed outcome, retaining no condition object or executable form."
  (agentcomms:json-object
   "outcome" (string-downcase (symbol-name outcome))
   "value" (when (eq outcome ':ok) (acp-extension-value value))
   "report" (when report (subseq report 0 (min 2000 (length report))))
   "conditionType" condition-type))

(-> acp-extension-capabilities () hash-table)
(defun acp-extension-capabilities ()
  "Describe only the installed typed extension handlers."
  (agentcomms:json-object
   "schemaVersion" 1
   "methods" (coerce (cons "_autolith/capabilities"
                            (loop for (name handler) in *acp-extension-methods*
                                  when (fboundp handler) collect name))
                      'vector)
   "features" (agentcomms:json-object
                "events" t "sessionScoped" t
                "turnEvents" (if (fboundp 'acp-session-run-turn) t (argo:json-false))
               "resourceSchemes" (if (fboundp 'acp-extension-resource)
                                     (vector "workspace" "shell-log") #())
               "automaticContinuation" (if (fboundp 'acp-completion-start)
                                           t (argo:json-false)))))

(-> acp-extension-notify (acp-session string &optional t) null)
(defun acp-extension-notify (session kind &optional data)
  "Send an ordered extension event without failing provider finalization on connection loss."
  (with-lock-held ((acp-session-extension-lock session))
    (unless (acp-session-closed-p session)
      (handler-case
          (agentcomms:agent-client-notify
           (acp-session-service session) "_autolith/event"
           (agentcomms:json-object
            "sessionId" (acp-session-identifier session)
            "sequence" (incf (acp-session-extension-sequence session))
            "kind" kind "data" (or data (agentcomms:json-object))))
        (agentcomms:acp-error () nil))))
  nil)

(-> acp-extension-turn-notify
    (acp-session string &key (:source string) (:outcome (option keyword))) null)
(defun acp-extension-turn-notify (session state &key source outcome)
  "Publish an owned turn boundary before callbacks or after their final flush."
  (acp-extension-notify
   session "turn"
   (agentcomms:json-object "state" state "epoch" (acp-session-epoch session)
                          "source" source
                          "outcome" (if outcome (string-downcase (symbol-name outcome)) ':null))))

(-> acp-extension--usage (acp-session) hash-table)
(defun acp-extension--usage (session)
  "Project measured request usage, using the durable prompt cache when replaying."
  (let* ((conversation (application-conversation (acp-session-application session)))
         (usage (with-lock-held ((acp-session-extension-lock session))
                  (acp-session-extension-usage session)))
         (cached-usage
           (with-recursive-lock-held ((conversation-append-lock conversation))
             (copy-tree (conversation-prompt-cache-usage conversation))))
         (input (conversation--usage-field (or usage cached-usage) "input_tokens"))
         (configuration (application-configuration (acp-session-application session))))
    (agentcomms:json-object
     "input" (or input ':null)
     "output" (or (conversation--usage-field usage "output_tokens") ':null)
     "cached" (or (conversation--usage-field (or usage cached-usage) "cached_input_tokens") ':null)
     "contextUsed" (or input ':null)
     "contextLimit" (or (provider-model-context-window-for (config :model configuration)) ':null)
     "contextMeasured" (if input t (argo:json-false)))))

(-> acp-extension-live-job-count (acp-session) integer)
(defun acp-extension-live-job-count (session)
  "Count nonterminal owned jobs without copying their potentially large progress."
  (let* ((application (acp-session-application session))
         (orchestrator (application--task-orchestrator application)))
    (if orchestrator
        (count-if-not #'job-terminal-p
                      (task-orchestrator-list-visible-jobs
                       orchestrator (application-agent application)))
        0)))

(-> acp-extension-state (acp-session hash-table) hash-table)
(defun acp-extension-state (session params)
  "Return current headless session state, marking unavailable measurements null."
  (declare (ignore params))
  (let* ((application (acp-session-application session))
         (configuration (application-configuration application))
         (conversation (application-conversation application))
         (turn (with-lock-held ((acp-session-lock session))
                 (cond
                   ((acp-session-closed-p session) "interrupted")
                   ((and (acp-session-prompt-thread session) (acp-session-cancelled-p session)) "cancelling")
                   ((acp-session-prompt-thread session) "running")
                   (t "idle")))))
    (agentcomms:json-object
     "workspace" (namestring (config :working-directory configuration))
     "title" (or (conversation-title conversation) ':null)
     "model" (config :model configuration)
     "effort" (config :reasoning-effort configuration)
     "permissionMode" (string-downcase (symbol-name (acp-session-mode session)))
     "turnState" turn "usage" (acp-extension--usage session)
     "liveJobCount" (acp-extension-live-job-count session)
     "planPresent" (if (plan-load configuration) t (argo:json-false))
     "health" (cond ((acp-session-closing-p session) "closing")
                    ((acp-session-completion-report session) "condition")
                    (t "ready"))
     "report" (or (acp-session-completion-report session) ':null))))

(-> acp-extension--call (acp-service list hash-table) t)
(defun acp-extension--call (service entry params)
  "Validate session authority before invoking ENTRY at its required admission boundary."
  (let* ((identifier (agentcomms:acp-field params "sessionId" :type ':string :required-p t))
         (session (acp-service--session service identifier))
         (application (acp-session-application session))
         (*active-application* application)
         (*configuration* (application-configuration application))
         (*default-pathname-defaults* (config :working-directory *configuration*))
         (handler (second entry)))
    (labels ((invoke ()
               "Own the handler through cancellation and resource retirement."
               (let ((thread (current-thread)))
                 (unwind-protect
                      (progn
                        (with-lock-held ((acp-service-lock service))
                          (when (acp-service-closed-p service)
                            (error 'agentcomms:acp-connection-closed))
                          (with-lock-held ((acp-session-lock session))
                            (when (or (acp-session-closing-p session) (acp-session-closed-p session))
                              (error 'acp-extension-unavailable :reason "The session is closing."))
                            (setf (gethash thread (acp-session-tool-threads session))
                                  (acp-session-epoch session))))
                        (funcall handler session params))
                   (with-lock-held ((acp-session-lock session))
                     (remhash thread (acp-session-tool-threads session)))))))
      (if (eq (third entry) ':write)
          (acp-service--call-with-operation
           service session
           (lambda ()
             (prog1 (invoke)
               (acp-extension-notify session "state"))))
          (invoke)))))

(defmethod agentcomms:agent-extension-request ((service acp-service) method params)
  "Dispatch negotiated session-scoped calls and return portable typed outcomes."
  (let ((entry (assoc method *acp-extension-methods* :test #'string=)))
    (unless (or entry (string= method "_autolith/capabilities"))
      (return-from agentcomms:agent-extension-request (call-next-method)))
    (handler-case
        (acp-extension-result
         ':ok :value (if entry
                         (acp-extension--call service entry params)
                         (acp-extension-capabilities)))
      (acp-operation-busy ()
        (acp-extension-result ':busy :report "Another primary operation is active."))
      (acp-extension-unavailable (condition)
        (acp-extension-result ':unsupported :report (acp-extension-unavailable-reason condition)))
      (application-turn-cancelled ()
        (acp-extension-result ':cancelled))
      (error (condition)
        (acp-extension-result ':condition :report (princ-to-string condition)
                              :condition-type (string-downcase (symbol-name (type-of condition))))))))

(defmethod agent-observer-status :after ((observer acp-observer) status details)
  "Publish measured usage and compaction notices after their ordinary ACP projection."
  (let ((session (acp-observer-session observer)))
    (case status
      (:provider-request-completed
       (with-lock-held ((acp-session-extension-lock session))
         (setf (acp-session-extension-usage session)
               (loop for name in '("input_tokens" "output_tokens" "cached_input_tokens")
                     collect (list name (conversation--usage-field (getf details :usage) name)))
               (gethash (config :model (application-configuration (acp-session-application session)))
                        (acp-session-extension-verified-models session))
               (get-universal-time)))
       (acp-extension-notify session "state"))
      (:compaction-completed
       (acp-extension-notify session "compaction"))
      (:tool-call-completed
       (acp-extension-notify session "jobs"))))
  nil)
