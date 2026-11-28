(in-package #:autolith)

(defparameter *application-client-methods*
    '((:client-state application-client-state :server)
      (:settings application-client-settings :server)
      (:models application-client-models :server)
      (:set-setting application-client-set-setting :server)
      (:operations application-client-operations :server)
      (:invoke application-client-invoke :server)
      (:conversations application-client-conversations :server)
      (:jobs application-client-jobs :server)
      (:job application-client-job :server)
      (:job-transcript application-client-job-transcript :server)
      (:job-cancel application-client-job-cancel :server)
      (:resource application-client-resource :server))
  "Native shared channel commands and their application handlers.")

;;;; -- Portable Client Values --

(define-condition application-client-unavailable (error)
  ((reason :initarg :reason :reader application-client-unavailable-reason
           :documentation "Why the requested headless operation is unavailable."))
  (:report (lambda (condition stream)
             (write-string (application-client-unavailable-reason condition) stream)))
  (:documentation "An extension operation has no admitted headless implementation."))

(-> application-client-value (t) t)
(defun application-client-value (value)
  "Copy native VALUE within bounded structural limits, preserving Lisp types."
  (handler-case
      (portable-value-map
       value
       (lambda (item child)
         (cond
           ((null item)
            nil)
           ((eq item t)
            t)
           ((stringp item)
            (when (> (length item) 262144)
              (error 'application-client-unavailable
                     :reason "The client string exceeds its character limit."))
            (copy-seq item))
           ((or (integerp item) (floatp item) (rationalp item))
            item)
           ((characterp item)
            item)
           ((pathnamep item)
            (pathname item))
           ((symbolp item)
            item)
           ((hash-table-p item)
            (let ((copy (make-hash-table :test (hash-table-test item))))
              (maphash (lambda (key member)
                         (setf (gethash (funcall child key) copy)
                               (funcall child member)))
                       item)
              copy))
           ((vectorp item)
            (map 'vector child item))
           (t
            (error 'application-client-unavailable
                   :reason "The value has no portable client representation."))))
       :containers-p t :proper-lists-p t :list #'identity
       :maximum-depth 32 :maximum-nodes 16384 :list-node-policy ':container)
    (portable-value-error (condition)
      (error 'application-client-unavailable
             :reason (if (eq (portable-value-error-reason condition) ':dotted-list)
                         "A client list is not proper."
                         "The client value exceeds its structural limit.")))))


;;;; -- Server Side --

;;; Semantic requests run against the channel's application in the world server.

(-> application-client--usage (application) list)
(defun application-client--usage (application)
  "Project measured request usage as a native plist without copying history."
  (let* ((conversation  (application-conversation application))
         (hub           (application-client-hub application))
         (usage         (with-lock-held ((client-hub-lock hub))
                          (client-hub-usage hub)))
         (cached-usage
           (with-recursive-lock-held ((conversation-append-lock conversation))
             (copy-tree (conversation-prompt-cache-usage conversation))))
         (input         (conversation--usage-field (or usage cached-usage) "input_tokens"))
         (configuration (application-configuration application)))
    (list :input              input
          :output             (conversation--usage-field usage "output_tokens")
          :cached             (conversation--usage-field (or usage cached-usage) "cached_input_tokens")
          :context-used       input
          :context-limit      (config :context-window configuration)
          :context-measured-p (not (null input)))))

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

(-> application-client--goal (application) list)
(defun application-client--goal (application)
  "Project the channel's current goal as native values."
  (let ((goal (application-goal application)))
    (when (mission-goal-p goal)
      (list :objective     (getf goal :objective)
            :status        (getf goal :status)
            :kind          ':mission
            :created-at    (getf goal :created-at)
            :continuations (getf goal :continuations 0)))))

(-> application-client-state (application) list)
(defun application-client-state (application)
  "Return the channel's current state as native Lisp values."
  (let ((configuration (application-configuration application))
        (conversation  (application-conversation application)))
    (list :workspace       (namestring (config :working-directory configuration))
          :title           (conversation-title conversation)
          :model           (config :model configuration)
          :effort          (config :reasoning-effort configuration)
          :permission-mode (application-permission-mode application)
          :turn-state      (intern (string-upcase (application-client--turn-state application)) :keyword)
          :usage           (application-client--usage application)
          :live-job-count  (application-client-live-job-count application)
          :goal            (application-client--goal application)
          :plan-present-p  (not (null (plan-load configuration)))
          :health          :ready
          :report          nil)))

(-> application-client-run (application keyword &rest t) list)
(defun application-client-run (application command &rest arguments)
  "Dispatch native COMMAND with Lisp ARGUMENTS and return its semantic outcome."
  (let ((entry (assoc command *application-client-methods*)))
    (unless entry
      (return-from application-client-run
        (list :outcome :unsupported :report
              (format nil "The world server does not serve ~S." command))))
    (handler-case
        (let ((*active-application* application)
              (*configuration*      (application-configuration application))
              (*default-pathname-defaults*
                (config :working-directory (application-configuration application))))
          (list :outcome :ok :value
                (apply (second entry) application arguments)))
      (application-client-operation-busy nil
        (list :outcome :busy :report "Another primary operation is active."))
      (application-client-operation-unavailable (condition)
        (list :outcome :unsupported :report (princ-to-string condition)))
      (application-client-unavailable (condition)
        (list :outcome :unsupported :report (application-client-unavailable-reason condition)))
      (application-turn-cancelled nil (list :outcome :cancelled))
      (error (condition)
        (list :outcome :condition :report (princ-to-string condition) :condition-type
              (string-downcase (symbol-name (type-of condition))))))))
