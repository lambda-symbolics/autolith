(in-package #:autolith)

;;;; -- Debugger Recovery --

(defparameter *application-debugger-recovery-names*
  '("AUTOLITH-RECOVERY-1" "AUTOLITH-RECOVERY-2" "AUTOLITH-RECOVERY-3")
  "The fixed restart names available to an application debugger session.")

(defparameter *application-debugger-snapshot* nil
  "The detached diagnostic snapshot bound during owner-thread restart selection.")

(define-condition application-debugger-cancelled (condition)
  ()
  (:documentation "Diagnosis was cancelled by its owner."))

(defclass application-debugger-session ()
  ((snapshot :initarg       :snapshot
             :reader        application-debugger-portable-snapshot
             :documentation "Detached library diagnostic data, with no live objects.")
   (application :initarg :application :initform nil
                :reader        application-debugger-application
                :documentation "The application requesting model diagnosis.")
   (lock :initform      (make-lock "Autolith application debugger")
         :reader        application-debugger-lock
         :documentation "The diagnosis state lock.")
   (condition-variable :initform      (make-condition-variable
                                       :name "Autolith debugger state")
                       :reader        application-debugger-condition-variable
                       :documentation "The diagnosis state change notification.")
   (diagnosis-thread :initform      nil
                     :accessor      application-debugger-diagnosis-thread
                     :documentation "The independent diagnosis thread.")
   (diagnosis-state :initform      :idle
                    :accessor      application-debugger-diagnosis-state
                    :documentation "The diagnosis lifecycle state.")
   (explanation :initform      nil
                :accessor      application-debugger-explanation
                :documentation "The bounded diagnosis explanation.")
   (failure :initform      nil
            :accessor      application-debugger-failure
            :documentation "The diagnosis failure condition, when any.")
   (proposals :initform      nil
              :accessor      application-debugger-proposals
              :documentation "The validated recovery proposals in reverse order.")
   (cancelled-p :initform      nil
                :accessor      application-debugger-cancelled-p
                :documentation "Whether the owner cancelled diagnosis."))
  (:documentation "An independent model diagnosis over a detached failure snapshot."))

(defclass application-debugger-propose-tool (tool)
  ((session :initarg       :session
            :reader        application-debugger-propose-tool-session
            :documentation "The diagnosis session accepting proposals."))
  (:documentation "Restricted debugger diagnosis tool for executable proposals."))

(defmethod tool-execute ((tool application-debugger-propose-tool)
                         (context tool-context) (arguments hash-table))
  "Validate and atomically store one model proposal."
  (declare (ignore context))
  (handler-case
      (let ((proposal
              (make-instance 'recovery
                             :kind               (find (tool-argument arguments "kind" :required t)
                                                       (recovery-kinds)
                                                       :key #'symbol-name :test #'string-equal)
                             :report             (bounded-string
                                                  (tool-argument arguments "report" :required t))
                             :restart-id         (tool-argument arguments "target-restart-id")
                             :preparation-source (let ((source (tool-argument arguments "preparation-source")))
                                                    (and source (bounded-string source)))
                             :argument-source    (let ((source (tool-argument arguments "argument-source")))
                                                   (and source (bounded-string source)))
                             :return-source      (let ((source (tool-argument arguments "return-source")))
                                                   (and source (bounded-string source))))))
        (let ((session (application-debugger-propose-tool-session tool)))
          (with-lock-held ((application-debugger-lock session))
            (when (application-debugger-cancelled-p session)
              (return-from tool-execute
                (tool-failure "Debugger diagnosis was cancelled.")))
            (when (>= (length (application-debugger-proposals session)) 3)
              (return-from tool-execute
                (tool-failure "At most three debugger proposals are allowed.")))
            (validate-recovery (application-debugger-portable-snapshot session)
                               proposal)
            (push proposal (application-debugger-proposals session))
            (condition-notify (application-debugger-condition-variable session)))
          (tool-success "Debugger proposal accepted.")))
    (recovery-error (condition)
      (tool-failure (princ-to-string condition)))
    (error (condition)
      (tool-failure (format nil "Invalid debugger proposal: ~A" condition)))))

(-> application-debugger--proposal-tool
    (application-debugger-session)
    application-debugger-propose-tool)
(defun application-debugger--proposal-tool (session)
  "Create the strict model-facing proposal tool for SESSION."
  (make-instance 'application-debugger-propose-tool
                 :namespace "debugger" :name "propose"
                 :description "Submit one executable recovery proposal. Submit no more than three."
                 :parameters
                 (tool-object-schema
                  (json-object
                   "kind" (json-object "type" "string"
                                       "enum" (map 'vector
                                                   (lambda (kind) (string-downcase (symbol-name kind)))
                                                   (recovery-kinds)))
                   "report" (tool-string-property "Why this executable proposal is appropriate.")
                   "target-restart-id" (tool-string-property "The exact restart id, when needed.")
                   "preparation-source" (tool-string-property "A portable source form run before invocation.")
                   "argument-source" (tool-string-property "A portable source form producing the restart argument.")
                   "return-source" (tool-string-property "A portable source form producing returned values."))
                  '("kind" "report"))
                 :session     session))

(-> application-debugger-start-diagnosis
    (application-debugger-session configuration)
    application-debugger-session)
(defun application-debugger-start-diagnosis (session configuration)
  "Start independent model diagnosis for SESSION using CONFIGURATION."
  (with-lock-held ((application-debugger-lock session))
    (when (and (application-debugger-diagnosis-thread session)
               (thread-alive-p (application-debugger-diagnosis-thread session)))
      (error 'application-debugger-busy-error
             :session session
             :message "Debugger diagnosis is already running."))
    (setf (application-debugger-diagnosis-state session) :running
          (application-debugger-cancelled-p session) nil
          (application-debugger-failure session) nil
          (application-debugger-explanation session) nil
          (application-debugger-proposals session) nil)
    (setf (application-debugger-diagnosis-thread session)
          (make-thread
           (lambda ()
             (let ((registry nil))
               (unwind-protect
                    (handler-case
                        (let* ((application
                                 (application-debugger-application session))
                               (provider
                                 (if (and application
                                          (application-provider application))
                                     (provider-with-configuration
                                      (application-provider application)
                                      configuration)
                                     (provider-create configuration)))
                               (conversation
                                 (conversation-create
                                  configuration
                                  :storage-root
                                  (configuration-inference-root configuration)))
                               (worker
                                 (and application
                                      (application-worker application)))
                               (text "")
                               (text-lock
                                 (make-lock "Debugger diagnosis text"))
                               (observer
                                 (callback-agent-observer-create
                                  :text-callback
                                  (lambda (delta)
                                    (with-lock-held (text-lock)
                                      (setf text
                                            (bounded-string
                                             (concatenate 'string text delta)
                                             :limit 8000)))))))
                          (setf registry
                                (application--create-tool-registry configuration))
                          (tool-registry-register
                           registry
                           (application-debugger--proposal-tool session))
                          (agent-run-user-turn
                           (agent-create :configuration configuration
                                         :provider      provider
                                         :conversation  conversation
                                         :tool-registry registry
                                         :worker        worker)
                           (format nil
                                   "You are diagnosing a suspended application operation.~%~%~A~%~%Explain the failure briefly and submit only executable recovery proposals using debugger.propose. Every proposal must contain source strings, never decoded arbitrary values."
                                   (with-output-to-string (stream)
                                     (write
                                      (application-debugger-portable-snapshot session)
                                      :stream stream)))
                           :observer           observer
                           :tools-p            t
                           :tool-restriction-p t
                           :tool-allowlist
                           '("resource.read" "search.files" "search.glob"
                             "search.content" "debugger.propose"))
                          (with-lock-held (text-lock)
                            (with-lock-held ((application-debugger-lock session))
                              (setf (application-debugger-explanation session)
                                    (bounded-string text)))))
                      (condition (condition)
                        (with-lock-held ((application-debugger-lock session))
                          (unless (application-debugger-cancelled-p session)
                            (setf (application-debugger-failure session) condition
                                  (application-debugger-diagnosis-state session)
                                  :failed)))))
                 (when registry
                   ; mallet:suppress no-ignore-errors -- best-effort cleanup of a finished diagnosis
                   (ignore-errors
                     (tool-registry-close-runtime-state registry)))
                 (with-lock-held ((application-debugger-lock session))
                   (unless (or (application-debugger-cancelled-p session)
                               (eq (application-debugger-diagnosis-state session)
                                   :failed))
                     (setf (application-debugger-diagnosis-state session)
                           :complete))
                   (condition-notify
                    (application-debugger-condition-variable session))))))
           :name "Autolith debugger diagnosis")))
  session)

(-> application-debugger-cancel-diagnosis
    (application-debugger-session)
    application-debugger-session)
(defun application-debugger-cancel-diagnosis (session)
  "Cancel diagnosis for SESSION and interrupt its diagnosis thread."
  (let ((thread nil))
    (with-lock-held ((application-debugger-lock session))
      (setf (application-debugger-cancelled-p session) t
            (application-debugger-diagnosis-state session) :cancelled
            thread (application-debugger-diagnosis-thread session))
      (condition-notify (application-debugger-condition-variable session)))
    (when (and thread (thread-alive-p thread))
      ; mallet:suppress no-ignore-errors -- the thread may exit before the interrupt arrives
      (ignore-errors
        (interrupt-thread thread
                          (lambda ()
                            (error 'application-debugger-cancelled))))
      (loop repeat 100
            while (thread-alive-p thread)
            do (sleep 0.01))
      (unless (thread-alive-p thread)
        ; mallet:suppress no-ignore-errors -- a cancelled thread ends abnormally; joining only reaps it
        (ignore-errors
          (join-thread thread))))
    session))

(-> application-debugger-poll (application-debugger-session) list)
(defun application-debugger-poll (session)
  "Return a synchronized portable diagnosis status plist for SESSION."
  (with-lock-held ((application-debugger-lock session))
    (list :state       (application-debugger-diagnosis-state session)
          :explanation (application-debugger-explanation session)
          :failure     (and (application-debugger-failure session)
                            (princ-to-string (application-debugger-failure session)))
          :proposals   (copy-list (application-debugger-proposals session))
          :cancelled-p (application-debugger-cancelled-p session))))


;;;; -- Conditions --

(define-condition application-debugger-busy-error (autolith-error)
  ((session
    :initarg       :session
    :reader        application-debugger-busy-error-session
    :type          application-debugger-session
    :documentation "The debugger session whose diagnosis thread is still running."))
  (:documentation "Diagnosis was requested while the session's previous diagnosis still runs."))
