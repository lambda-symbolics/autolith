(in-package #:autolith)

;;;; -- Debugger Recovery --

(defparameter *application-debugger-recovery-names*
  '("AUTOLITH-RECOVERY-1" "AUTOLITH-RECOVERY-2" "AUTOLITH-RECOVERY-3")
  "The fixed restart names available to an application debugger session.")

(defparameter *application-debugger-source* ""
  "The exact operation source visible to the current owner-thread debugger.")

(defparameter *application-debugger-operation-kind* ':lisp
  "The current owner-thread debugger operation kind.")

(defparameter *application-debugger-retry-p* t
  "Whether the current owner-thread debugger may retry the whole operation.")

(defparameter *application-debugger-return-values-p* nil
  "Whether the current owner-thread debugger may return replacement values.")

(defclass application-debugger-recovery ()
  ((kind :initarg :kind :reader application-debugger-recovery-kind
         :documentation "The proposed recovery operation kind.")
   (report :initarg :report :reader application-debugger-recovery-report
           :documentation "The model's concise explanation of the proposal.")
   (restart-id :initarg :restart-id :initform nil
               :reader application-debugger-recovery-restart-id
               :documentation "The portable restart identifier, when applicable.")
   (preparation-source :initarg :preparation-source :initform nil
                       :reader application-debugger-recovery-preparation-source
                       :documentation "Source for the repair preparation form.")
   (argument-source :initarg :argument-source :initform nil
                    :reader application-debugger-recovery-argument-source
                    :documentation "Source for the restart argument form.")
   (return-source :initarg :return-source :initform nil
                  :reader application-debugger-recovery-return-source
                  :documentation "Source for the returned values form."))
  (:documentation "A validated, portable executable recovery proposal."))

(define-condition application-debugger-recovery-error (error)
  ((proposal :initarg :proposal :reader application-debugger-recovery-error-proposal
             :documentation "The invalid recovery proposal, when available.")
   (kind :initarg :kind :reader application-debugger-recovery-error-kind
         :documentation "The recovery kind associated with the failure.")
   (reason :initarg :reason :reader application-debugger-recovery-error-reason
           :documentation "The structured reason for rejecting the proposal."))
  (:report (lambda (condition stream)
            (format stream "Debugger recovery proposal ~S (~S) is invalid: ~A."
                    (application-debugger-recovery-error-proposal condition)
                    (application-debugger-recovery-error-kind condition)
                    (application-debugger-recovery-error-reason condition))))
  (:documentation "A typed validation failure for an application debugger proposal."))

(define-condition application-debugger-cancelled (condition)
  ()
  (:documentation "Diagnosis was cancelled by its owner."))

(defclass application-debugger-session ()
  ((condition-type :initarg :condition-type
                   :reader application-debugger-condition-type
                   :documentation "The condition's actual type name.")
   (condition-report :initarg :condition-report
                     :reader application-debugger-condition-report
                     :documentation "The bounded condition report.")
   (source :initarg :source
           :reader application-debugger-source
           :documentation "The bounded source of the operation.")
   (operation-kind :initarg :operation-kind
                   :reader application-debugger-operation-kind
                   :documentation "The operation kind.")
   (owner-thread :initarg :owner-thread
                 :reader application-debugger-owner-thread
                 :documentation "The thread suspended at the failure.")
   (application :initarg :application :initform nil
                :reader application-debugger-application
                :documentation "The owning application, when diagnosis has one.")
   (restarts :initarg :restarts
             :reader application-debugger-restarts
             :documentation "The portable ordered restart descriptions.")
   (capabilities :initarg :capabilities
                 :reader application-debugger-capabilities
                 :documentation "The operation's supported recovery kinds.")
   (backtrace :initarg :backtrace
              :reader application-debugger-backtrace
              :documentation "The argument-free owner-thread backtrace snapshot.")
   (lock :initform (make-lock "Autolith application debugger")
         :reader application-debugger-lock
         :documentation "The diagnosis state lock.")
   (condition-variable :initform (make-condition-variable
                                  :name "Autolith debugger state")
                       :reader application-debugger-condition-variable
                       :documentation "The diagnosis state change notification.")
   (diagnosis-thread :initform nil
                     :accessor application-debugger-diagnosis-thread
                     :documentation "The independent diagnosis thread.")
   (diagnosis-state :initform :idle
                    :accessor application-debugger-diagnosis-state
                    :documentation "The diagnosis lifecycle state.")
   (explanation :initform nil
                :accessor application-debugger-explanation
                :documentation "The bounded diagnosis explanation.")
   (failure :initform nil
            :accessor application-debugger-failure
            :documentation "The diagnosis failure condition, when any.")
   (proposals :initform nil
              :accessor application-debugger-proposals
              :documentation "The validated recovery proposals in reverse order.")
   (cancelled-p :initform nil
                :accessor application-debugger-cancelled-p
                :documentation "Whether the owner cancelled diagnosis."))
  (:documentation "Suspended owner-thread debugger state and independent diagnosis state."))

(-> application-debugger--portable-p (t) boolean)
(defun application-debugger--portable-p (object)
  "Return true when OBJECT is a portable tree of debugger data."
  (or (null object) (stringp object) (numberp object) (symbolp object)
      (and (consp object) (every #'application-debugger--portable-p object))
      (and (vectorp object)
           (every #'application-debugger--portable-p object))))

(-> application-debugger-portable-snapshot (application-debugger-session) list)
(defun application-debugger-portable-snapshot (session)
  "Build a bounded portable diagnostic snapshot from SESSION."
  (list :condition-type (application-debugger-condition-type session)
        :condition-report (application-debugger-condition-report session)
        :restarts (mapcar (lambda (restart)
                            (list :id (getf restart :id)
                                  :report (getf restart :report)))
                          (application-debugger-restarts session))
        :source (application-debugger-source session)
        :operation-kind (application-debugger-operation-kind session)
        :backtrace (application-debugger-backtrace session)
        :capabilities (application-debugger-capabilities session)))

(-> application-debugger--valid-kind-p (t) boolean)
(defun application-debugger--valid-kind-p (kind)
  "Return true for a supported recovery KIND."
  (not
   (null
    (member kind '(:invoke-restart :repair-and-invoke :retry-operation
                   :repair-and-retry :return-values :abort-operation)
            :test #'eq))))

(-> application-debugger--recovery-error
    (t t string)
    null)
(defun application-debugger--recovery-error (proposal kind reason)
  "Signal a typed validation error for PROPOSAL, KIND, and REASON."
  (error 'application-debugger-recovery-error
         :proposal proposal
         :kind kind
         :reason reason))

(-> application-debugger--nonempty-source-p (t) boolean)
(defun application-debugger--nonempty-source-p (source)
  "Return true when SOURCE is a nonempty bounded source string."
  (and (stringp source) (plusp (length source))))

(-> application-debugger--validate-recovery
    (application-debugger-session application-debugger-recovery)
    application-debugger-recovery)
(defun application-debugger--validate-recovery (session recovery)
  "Validate RECOVERY against SESSION's portable restart and capability snapshot."
  (unless (typep recovery 'application-debugger-recovery)
    (application-debugger--recovery-error
     recovery nil "proposal is not a recovery proposal"))
  (let* ((kind
           (application-debugger-recovery-kind recovery))
         (restart-id
           (application-debugger-recovery-restart-id recovery))
         (preparation-source
           (application-debugger-recovery-preparation-source recovery))
         (argument-source
           (application-debugger-recovery-argument-source recovery))
         (return-source
           (application-debugger-recovery-return-source recovery))
         (restart-ids
           (mapcar (lambda (restart)
                     (getf restart :id))
                   (application-debugger-restarts session))))
    (unless (application-debugger--valid-kind-p kind)
      (application-debugger--recovery-error
       recovery kind "unsupported recovery kind"))
    (unless (and (stringp (application-debugger-recovery-report recovery))
                 (plusp (length (application-debugger-recovery-report recovery))))
      (application-debugger--recovery-error
       recovery kind "report must be a nonempty string"))
    (dolist (source (list preparation-source argument-source return-source))
      (when (and source
                 (not (application-debugger--nonempty-source-p source)))
        (application-debugger--recovery-error
         recovery kind "source fields must be nonempty strings")))
    (flet ((require-restart ()
             "Require a valid portable restart identifier."
             (unless (and (stringp restart-id)
                          (member restart-id restart-ids :test #'string=))
               (application-debugger--recovery-error
                recovery kind
                "target-restart-id is not a valid diagnostic restart id")))

           (require-capability ()
             "Require SESSION to support KIND."
             (unless (getf (application-debugger-capabilities session) kind)
               (application-debugger--recovery-error
                recovery kind "operation does not support this recovery"))))
      (case kind
        (:invoke-restart
         (require-restart)
         (when (or preparation-source return-source)
           (application-debugger--recovery-error
            recovery kind
            "preparation-source and return-source are not allowed")))
        (:repair-and-invoke
         (require-restart)
         (unless (application-debugger--nonempty-source-p preparation-source)
           (application-debugger--recovery-error
            recovery kind "preparation-source must be nonempty"))
         (when return-source
           (application-debugger--recovery-error
            recovery kind "return-source is not allowed")))
        (:retry-operation
         (require-capability)
         (when (or restart-id preparation-source argument-source return-source)
           (application-debugger--recovery-error
            recovery kind "target and source fields are not allowed")))
        (:repair-and-retry
         (require-capability)
         (unless (application-debugger--nonempty-source-p preparation-source)
           (application-debugger--recovery-error
            recovery kind "preparation-source must be nonempty"))
         (when (or restart-id argument-source return-source)
           (application-debugger--recovery-error
            recovery kind
            "target, argument-source, and return-source are not allowed")))
        (:return-values
         (require-capability)
         (unless (application-debugger--nonempty-source-p return-source)
           (application-debugger--recovery-error
            recovery kind "return-source must be nonempty"))
         (when (or restart-id preparation-source argument-source)
           (application-debugger--recovery-error
            recovery kind
            "target and preparation/argument sources are not allowed")))
        (:abort-operation
         (when (or restart-id preparation-source argument-source return-source)
           (application-debugger--recovery-error
            recovery kind "target and source fields are not allowed")))))
    recovery))

(defclass application-debugger-propose-tool (tool)
  ((session :initarg :session
            :reader application-debugger-propose-tool-session
            :documentation "The diagnosis session accepting proposals."))
  (:documentation "Restricted debugger diagnosis tool for executable proposals."))

(defmethod tool-execute ((tool application-debugger-propose-tool)
                         (context tool-context) (arguments hash-table))
  "Validate and atomically store one model proposal."
  (declare (ignore context))
  (handler-case
      (let ((proposal
              (make-instance 'application-debugger-recovery
                             :kind (intern (string-upcase
                                            (tool-argument arguments "kind" :required t))
                                           :keyword)
                             :report (bounded-string
                                      (tool-argument arguments "report" :required t))
                             :restart-id (tool-argument arguments "target-restart-id")
                             :preparation-source (let ((source (tool-argument arguments "preparation-source")))
                                                    (and source (bounded-string source)))
                             :argument-source (let ((source (tool-argument arguments "argument-source")))
                                                (and source (bounded-string source)))
                             :return-source (let ((source (tool-argument arguments "return-source")))
                                              (and source (bounded-string source))))))
        (let ((session (application-debugger-propose-tool-session tool)))
          (with-lock-held ((application-debugger-lock session))
            (when (application-debugger-cancelled-p session)
              (return-from tool-execute
                (tool-failure "Debugger diagnosis was cancelled.")))
            (when (>= (length (application-debugger-proposals session)) 3)
              (return-from tool-execute
                (tool-failure "At most three debugger proposals are allowed.")))
            (application-debugger--validate-recovery session proposal)
            (push proposal (application-debugger-proposals session))
            (condition-notify (application-debugger-condition-variable session)))
          (tool-success "Debugger proposal accepted.")))
    (application-debugger-recovery-error (condition)
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
                   "kind" (json-object "type" "string" "enum" #("invoke-restart" "repair-and-invoke" "retry-operation" "repair-and-retry" "return-values" "abort-operation"))
                   "report" (tool-string-property "Why this executable proposal is appropriate.")
                   "target-restart-id" (tool-string-property "The exact restart id, when needed.")
                   "preparation-source" (tool-string-property "A portable source form run before invocation.")
                   "argument-source" (tool-string-property "A portable source form producing the restart argument.")
                   "return-source" (tool-string-property "A portable source form producing returned values."))
                   '("kind" "report"))
                 :session session))

(-> application-debugger-start-diagnosis
    (application-debugger-session configuration)
    application-debugger-session)
(defun application-debugger-start-diagnosis (session configuration)
  "Start independent model diagnosis for SESSION using CONFIGURATION."
  (with-lock-held ((application-debugger-lock session))
    (when (and (application-debugger-diagnosis-thread session)
               (thread-alive-p (application-debugger-diagnosis-thread session)))
      (error "Debugger diagnosis is already running."))
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
                                         :provider provider
                                         :conversation conversation
                                         :tool-registry registry
                                         :worker worker)
                           (format nil
                                   "You are diagnosing a suspended application operation.~%~%~A~%~%Explain the failure briefly and submit only executable recovery proposals using debugger.propose. Every proposal must contain source strings, never decoded arbitrary values."
                                   (with-output-to-string (stream)
                                     (write
                                      (application-debugger-portable-snapshot session)
                                      :stream stream)))
                           :observer observer
                           :tools-p t
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
      (ignore-errors
        (interrupt-thread thread
                          (lambda ()
                            (error 'application-debugger-cancelled))))
      (loop repeat 100
            while (thread-alive-p thread)
            do (sleep 0.01))
      (unless (thread-alive-p thread)
        (ignore-errors
          (join-thread thread))))
    session))

(-> application-debugger-poll (application-debugger-session) list)
(defun application-debugger-poll (session)
  "Return a synchronized portable diagnosis status plist for SESSION."
  (with-lock-held ((application-debugger-lock session))
    (list :state (application-debugger-diagnosis-state session)
          :explanation (application-debugger-explanation session)
          :failure (and (application-debugger-failure session)
                        (princ-to-string (application-debugger-failure session)))
          :proposals (copy-list (application-debugger-proposals session))
          :cancelled-p (application-debugger-cancelled-p session))))
