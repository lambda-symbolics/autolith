(in-package #:autolith)

(defparameter *application-client-methods*
  '(("_autolith/state" application-client-state :server)
    ("_autolith/settings" application-client-settings :server)
    ("_autolith/models" application-client-models :server)
    ("_autolith/set-setting" application-client-set-setting :server)
    ("_autolith/operations" application-client-operations :server)
    ("_autolith/invoke" application-client-invoke :server)
    ("_autolith/conversations" application-client-conversations :server)
    ("_autolith/jobs" application-client-jobs :server)
    ("_autolith/job" application-client-job :server)
    ("_autolith/job-transcript" application-client-job-transcript :server)
    ("_autolith/job-send" application-client-job-send :server)
    ("_autolith/job-cancel" application-client-job-cancel :server)
    ("_autolith/resource" application-client-resource :server))
  "Shared channel semantic methods and their application handlers.")

;;;; -- Portable Client Values --

(define-condition application-client-unavailable (error)
  ((reason :initarg :reason :reader application-client-unavailable-reason
           :documentation "Why the requested headless operation is unavailable."))
  (:report (lambda (condition stream)
             (write-string (application-client-unavailable-reason condition) stream)))
  (:documentation "An extension operation has no admitted headless implementation."))

(-> application-client-value (t) t)
(defun application-client-value (value)
  "Copy portable VALUE into bounded JSON data without printing opaque runtime objects."
  (handler-case
      (portable-value-map
       value
       (lambda (item child)
         (cond
           ((null item)
            ':null)
           ((eq item t)
            t)
           ((argo:json-false-p item)
            (argo:json-false))
           ((stringp item)
            (when (> (length item) 262144)
              (error 'application-client-unavailable :reason "The extension string exceeds its character limit."))
            (copy-seq item))
           ((or (integerp item) (floatp item))
            item)
           ((pathnamep item)
            (namestring item))
           ((keywordp item)
            (if (eq item ':null) ':null (string-downcase (symbol-name item))))
           ((hash-table-p item)
            (let ((copy (make-hash-table :test #'equal)))
              (maphash (lambda (key member)
                         (unless (stringp key)
                           (error 'application-client-unavailable :reason "An extension object has a non-string key."))
                         (setf (gethash (funcall child key) copy) (funcall child member)))
                       item)
              copy))
           ((vectorp item)
            (map 'vector child item))
           (t
            (error 'application-client-unavailable :reason "The value has no portable extension representation."))))
       :containers-p t :proper-lists-p t :list (lambda (items) (coerce items 'vector))
       :maximum-depth 32 :maximum-nodes 16384 :list-node-policy ':container)
    (portable-value-error (condition)
      (error 'application-client-unavailable
             :reason (if (eq (portable-value-error-reason condition) ':dotted-list)
                         "An extension list is not proper."
                         "The extension value exceeds its structural limit.")))))


;;;; -- Server Side --

;;; Semantic requests run against the channel's application in the world server.

(-> application-client--usage (application) hash-table)
(defun application-client--usage (application)
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

(-> application-client-live-job-count (application) integer)
(defun application-client-live-job-count (application)
  "Count nonterminal owned jobs without copying their potentially large progress."
  (let ((orchestrator (application--task-orchestrator application)))
    (if orchestrator
        (count-if-not #'job-terminal-p
                      (task-orchestrator-list-visible-jobs
                       orchestrator (application-agent application)))
        0)))

(-> application-client--turn-state (application) string)
(defun application-client--turn-state (application)
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

(-> application-client-state (application hash-table) hash-table)
(defun application-client-state (application params)
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
     "turnState" (application-client--turn-state application)
     "usage" (application-client--usage application)
     "liveJobCount" (application-client-live-job-count application)
     "planPresent" (if (plan-load configuration) t (argo:json-false))
     "health" "ready"
     "report" ':null)))

(-> application-client-run (application string hash-table) list)
(defun application-client-run (application method params)
  "Dispatch shared channel METHOD and return its portable semantic outcome."
  (let ((entry (assoc method *application-client-methods* :test #'string=)))
    (unless entry
      (return-from application-client-run
        (list :outcome ':unsupported :report
              (format nil "The world server does not serve ~A." method))))
    (handler-case
     (let ((*active-application* application)
           (*configuration*      (application-configuration application))
           (*default-pathname-defaults*
            (config :working-directory (application-configuration application))))
       (list :outcome ':ok :value
             (application-client-value (funcall (second entry) application params))))
     (application-client-operation-busy nil
      (list :outcome ':busy :report "Another primary operation is active."))
     (application-client-operation-unavailable (condition)
      (list :outcome ':unsupported :report (princ-to-string condition)))
     (application-client-unavailable (condition)
      (list :outcome ':unsupported :report (application-client-unavailable-reason condition)))
     (application-turn-cancelled nil (list :outcome ':cancelled))
     (error (condition)
            (list :outcome ':condition :report (princ-to-string condition) :condition-type
                  (string-downcase (symbol-name (type-of condition))))))))
