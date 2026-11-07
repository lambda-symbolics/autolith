(in-package #:autolith)

;;;; -- Typed ACP Extensions --

(define-condition acp-extension-unavailable (error)
  ((reason :initarg :reason :reader acp-extension-unavailable-reason
           :documentation "Why the requested headless operation is unavailable."))
  (:report (lambda (condition stream)
             (write-string (acp-extension-unavailable-reason condition) stream)))
  (:documentation "An extension operation has no admitted headless implementation."))

(defparameter *acp-extension-methods*
  '(("_autolith/state" acp-extension-state :server)
    ("_autolith/settings" acp-extension-settings :server)
    ("_autolith/models" acp-extension-models :server)
    ("_autolith/set-setting" acp-extension-set-setting :server)
    ("_autolith/operations" acp-extension-operations :server)
    ("_autolith/invoke" acp-extension-invoke :server)
    ("_autolith/steer" acp-extension-steer :client)
    ("_autolith/queue" acp-extension-queue :client)
    ("_autolith/conversations" acp-extension-conversations :server)
    ("_autolith/jobs" acp-extension-jobs :server)
    ("_autolith/job" acp-extension-job :server)
    ("_autolith/job-transcript" acp-extension-job-transcript :server)
    ("_autolith/job-send" acp-extension-job-send :server)
    ("_autolith/job-cancel" acp-extension-job-cancel :server)
    ("_autolith/resource" acp-extension-resource :server))
  "Implemented session methods, their handlers, and where they run.

A :SERVER handler takes the channel's application and the request's
parameters and runs in the world server; a :CLIENT handler takes the ACP
session and runs in the ACP process.")

(-> acp-extension-value (t) t)
(defun acp-extension-value (value)
  "Copy portable VALUE into bounded JSON data without printing opaque runtime objects."
  (let ((nodes 0))
    (labels ((convert (item depth)
               "Convert one value within the document's shared structural bounds."
               (when (or (> (incf nodes) 16384) (> depth 32))
                 (error 'acp-extension-unavailable :reason "The extension value exceeds its structural limit."))
               (cond
                 ((null item)
                  ':null)
                 ((eq item t)
                  t)
                 ((argo:json-false-p item)
                  (argo:json-false))
                 ((stringp item)
                  (when (> (length item) 262144)
                    (error 'acp-extension-unavailable :reason "The extension string exceeds its character limit."))
                  (copy-seq item))
                 ((or (integerp item) (floatp item))
                  item)
                 ((pathnamep item)
                  (namestring item))
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
                "turnEvents" t
                "resourceSchemes" (vector "workspace" "shell-log")
                "automaticContinuation" t)))


;;;; -- Server Side --

;;; The editor's extension requests run in the world server, in the channel
;;; that hosts the session, against the channel's application.

(-> acp-extension--usage (application) hash-table)
(defun acp-extension--usage (application)
  "Project measured request usage, using the durable prompt cache when nothing was measured yet."
  (let* ((conversation  (application-conversation application))
         (hub           (application-client-hub application))
         (usage         (with-lock-held ((client-hub-lock hub))
                          (client-hub-usage hub)))
         (cached-usage
           (with-recursive-lock-held ((conversation-append-lock conversation))
             (copy-tree (conversation-prompt-cache-usage conversation))))
         (input         (conversation--usage-field (or usage cached-usage) "input_tokens"))
         (configuration (application-configuration application)))
    (agentcomms:json-object
     "input" (or input ':null)
     "output" (or (conversation--usage-field usage "output_tokens") ':null)
     "cached" (or (conversation--usage-field (or usage cached-usage) "cached_input_tokens") ':null)
     "contextUsed" (or input ':null)
     "contextLimit" (config :context-window configuration)
     "contextMeasured" (if input t (argo:json-false)))))

(-> acp-extension-live-job-count (application) integer)
(defun acp-extension-live-job-count (application)
  "Count nonterminal owned jobs without copying their potentially large progress."
  (let ((orchestrator (application--task-orchestrator application)))
    (if orchestrator
        (count-if-not #'job-terminal-p
                      (task-orchestrator-list-visible-jobs
                       orchestrator (application-agent application)))
        0)))

(-> acp-extension--turn-state (application) string)
(defun acp-extension--turn-state (application)
  "Return whether APPLICATION's channel is running a turn, cancelling one, or idle."
  (let ((controller (application-input-controller application)))
    (cond
      ((null controller)
       "idle")
      ((application-input-controller-turn-cancellation-p controller)
       "cancelling")
      ((application-input-controller-turn-active-p controller)
       "running")
      (t
       "idle"))))

(-> acp-extension-state (application hash-table) hash-table)
(defun acp-extension-state (application params)
  "Return the channel's current state, marking unavailable measurements null."
  (declare (ignore params))
  (let ((configuration (application-configuration application))
        (conversation  (application-conversation application)))
    (agentcomms:json-object
     "workspace" (namestring (config :working-directory configuration))
     "title" (or (conversation-title conversation) ':null)
     "model" (config :model configuration)
     "effort" (config :reasoning-effort configuration)
     "permissionMode" (string-downcase (symbol-name (application-permission-mode application)))
     "turnState" (acp-extension--turn-state application)
     "usage" (acp-extension--usage application)
     "liveJobCount" (acp-extension-live-job-count application)
     "planPresent" (if (plan-load configuration) t (argo:json-false))
     "health" "ready"
     "report" ':null)))

(-> acp-extension-run (application string hash-table) list)
(defun acp-extension-run (application method params)
  "Run extension METHOD with PARAMS in APPLICATION's channel and return its typed outcome.

The outcome is a plist: :OUTCOME, one of :OK, :BUSY, :UNSUPPORTED, :CANCELLED
or :CONDITION, with the :VALUE of a success, or a :REPORT and :CONDITION-TYPE."
  (let ((entry (assoc method *acp-extension-methods* :test #'string=)))
    (unless (and entry (not (eq (third entry) ':client)))
      (return-from acp-extension-run
        (list :outcome ':unsupported :report (format nil "The world server does not serve ~A." method))))
    (handler-case
        (let ((*active-application*        application)
              (*configuration*             (application-configuration application))
              (*default-pathname-defaults* (config :working-directory (application-configuration application))))
          (list :outcome ':ok
                :value   (acp-extension-value (funcall (second entry) application params))))
      (acp-operation-busy ()
        (list :outcome ':busy :report "Another primary operation is active."))
      (acp-extension-unavailable (condition)
        (list :outcome ':unsupported :report (acp-extension-unavailable-reason condition)))
      (application-turn-cancelled ()
        (list :outcome ':cancelled))
      (error (condition)
        (list :outcome        ':condition
              :report         (princ-to-string condition)
              :condition-type (string-downcase (symbol-name (type-of condition))))))))
