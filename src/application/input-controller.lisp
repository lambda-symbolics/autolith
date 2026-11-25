(in-package #:autolith)

;;;; -- Responsive Terminal Input --

(defparameter *application-interrupted-queue-notice-seconds* 8
  "Seconds to show a turn cancellation or queued-work pause notice.")

(defvar *application-turn-cancellation-deliverable-p* nil
  "True on a channel thread while its current work runs inside the cancellation handler.")

(defclass application-input-controller ()
  ((application
    :initarg       :application
    :reader        application-input-controller-application
    :type          application
    :documentation "The application receiving terminal events and submitted work.")
   (lock
    :initform      (make-lock "Autolith input controller")
    :reader        application-input-controller-lock
    :type          t
    :documentation "The lock protecting work, reader, and exit state.")
   (publication-lock
    :initform      (make-lock "Autolith input publication")
    :reader        application-input-controller-publication-lock
    :type          t
    :documentation "The lock serializing terminal and durable pending publication.")
   (publication-generation
    :initform      0
    :accessor      application-input-controller-publication-generation
    :type          (integer 0)
    :documentation "The latest detached external-publication generation.")
   (published-ui-generation
    :initform      0
    :accessor      application-input-controller-published-ui-generation
    :type          (integer 0)
    :documentation "The newest generation admitted to terminal publication.")
   (published-pending-generation
    :initform      0
    :accessor      application-input-controller-published-pending-generation
    :type          (integer 0)
    :documentation "The newest generation admitted to durable publication.")
   (durable-pending-generation
    :initform      0
    :accessor      application-input-controller-durable-pending-generation
    :type          (integer 0)
    :documentation "The newest successfully completed durable pending publication.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith input controller")
    :reader        application-input-controller-condition-variable
    :type          t
    :documentation "The main and reader thread wakeup condition.")
   (idle-wakeup-armed-p
    :initform      nil
    :accessor      application-input-controller-idle-wakeup-armed-p
    :type          boolean
    :documentation "Whether the owner has worked since its last idle wakeup.")
   (work-items
    :initform      (make-deque)
    :reader        application-input-controller-work-items
    :type          deque
    :documentation "FIFO message and command work submitted by the reader.")
   (initial-work-items
    :initarg       :initial-work-items
    :initform      (make-deque)
    :reader        application-input-controller-initial-work-items
    :type          deque
    :documentation "Ordered startup work excluded from durable pending input.")
   (project-adaptation-offer-p
    :initarg       :project-adaptation-offer-p
    :initform      nil
    :accessor      application-input-controller-project-adaptation-offer-p
    :type          boolean
    :documentation "Whether one attached-client project adaptation offer remains deferred until a completed turn reaches genuine idle.")
   (steering-items
    :initform      (make-deque)
    :reader        application-input-controller-steering-items
    :type          deque
    :documentation "FIFO user messages waiting for the active turn's next tool boundary.")
   (steering-in-flight-items
    :initform (make-deque)
    :reader   application-input-controller-steering-in-flight-items
    :type     deque
    :documentation
    "Identified steering messages drained but not yet durably acknowledged.")
   (pending-apply-items
    :initform (make-deque)
    :reader   application-input-controller-pending-apply-items
    :type     deque
    :documentation
    "FIFO command inputs applying at the active turn's next safe boundary.")
   (follow-up-edit-index
    :initform nil
    :accessor application-input-controller-follow-up-edit-index
    :type     (option (integer 0))
    :documentation
    "The recalled draft's position in the virtual queue that still includes it.")
   (follow-up-edit-work
    :initform      nil
    :accessor      application-input-controller-follow-up-edit-work
    :type          (option list)
    :documentation "The queued work item currently recalled into the draft.")
   (primary-steering-p
    :initform nil
    :accessor application-input-controller-primary-steering-p
    :type     boolean
    :documentation
    "Whether the active work consumes prose steering despite its kind.")
   (active-p
    :initform      nil
    :accessor      application-input-controller-active-p
    :type          boolean
    :documentation "Whether the main thread is processing one work item.")
   (active-work-kind
    :initform nil
    :accessor application-input-controller-active-work-kind
    :type     (option keyword)
    :documentation
    "The kind of work currently executing, independent of durable message state.")
   (active-work
    :initform      nil
    :accessor      application-input-controller-active-work
    :type          (option list)
    :documentation "The ordinary pending work item currently being dispatched.")
   (active-work-identifier
    :initform      nil
    :accessor      application-input-controller-active-work-identifier
    :type          (option string)
    :documentation "The durable identifier of ACTIVE-WORK, when present.")
   (active-promoted-steering-p
    :initform      nil
    :accessor      application-input-controller-active-promoted-steering-p
    :type          boolean
    :documentation "Whether the active queued message came from the steering prefix.")
   (active-work-interactive-p
    :initform      nil
    :accessor      application-input-controller-active-work-interactive-p
    :type          boolean
    :documentation "Whether ACTIVE-WORK came from the interactive prompt queue.")
   (prompt-marker-work-p
    :initform      nil
    :accessor      application-input-controller-prompt-marker-work-p
    :type          boolean
    :documentation "Whether current work emitted an OSC 133 execution-start marker.")
   (prompt-marker-status
    :initform      0
    :accessor      application-input-controller-prompt-marker-status
    :type          (integer 0)
    :documentation "The completion status for current marked prompt work.")
   (completed-turn-p
    :initform      nil
    :accessor      application-input-controller-completed-turn-p
    :type          boolean
    :documentation "Whether current work completed an ordinary model turn.")
   (prompt-marker-reopen-p
    :initform      nil
    :accessor      application-input-controller-prompt-marker-reopen-p
    :type          boolean
    :documentation "Whether current work may open the next semantic prompt block.")
   (pending-snapshot-identifier
    :initform      nil
    :accessor      application-input-controller-pending-snapshot-identifier
    :type          (option string)
    :documentation "The stable identifier of the current nonempty pending snapshot.")
   (vault-capture-identifiers
    :initform nil
    :accessor application-input-controller-vault-capture-identifiers
    :type     list
    :documentation
    "Vault captures transactionally represented by the current pending snapshot.")
   (vault-restore-work-items
    :initform      nil
    :accessor      application-input-controller-vault-restore-work-items
    :type          list
    :documentation "Recovered work staged for durable publication before queue admission.")
   (vault-restore-capture-identifiers
    :initform      nil
    :accessor      application-input-controller-vault-restore-capture-identifiers
    :type          list
    :documentation "Vault capture identifiers staged with pending recovered work.")
   (steering-promotion-prefix-count
    :initform 0
    :accessor application-input-controller-steering-promotion-prefix-count
    :type     (integer 0)
    :documentation
    "Queued work that must remain ahead of steering submitted during active work.")
   (pending-persistence-enabled-p
    :initarg  :pending-persistence-enabled-p
    :initform t
    :accessor application-input-controller-pending-persistence-enabled-p
    :type     boolean
    :documentation
    "Whether this controller may replace its conversation's pending snapshot.")
   (live-vault-sync-p
    :initform nil
    :accessor application-input-controller-live-vault-sync-p
    :type     boolean
    :documentation
    "Whether this session accepted new input that should create a live vault capture.")
   (queued-work-paused-p
    :initform nil
    :accessor application-input-controller-queued-work-paused-p
    :type     boolean
    :documentation
    "Whether an explicit interruption is holding queued work for user review.")
   (turn-cancellation-p
    :initform      nil
    :accessor      application-input-controller-turn-cancellation-p
    :type          boolean
    :documentation "Whether one active turn is cancelling but not yet finished.")
   (turn-cancellation-delivery-pending-p
    :initform      nil
    :accessor      application-input-controller-turn-cancellation-delivery-pending-p
    :type          boolean
    :documentation "Whether the main thread must still receive turn cancellation.")
   (stopping-p
    :initform      nil
    :accessor      application-input-controller-stopping-p
    :type          boolean
    :documentation "Whether no more terminal input or work may be accepted.")
   (exit-reason
    :initform      nil
    :accessor      application-input-controller-exit-reason
    :type          (option keyword)
    :documentation "The user-facing reason input processing stopped.")
   (input-pump
    :reader        application-input-controller-input-pump
    :type          input-pump
    :documentation "The restartable terminal reader thread and its pauses.")
   (main-thread
    :initarg       :main-thread
    :reader        application-input-controller-main-thread
    :type          t
    :documentation "The model and command thread interrupted for immediate exit.")
   (failure
    :initform      nil
    :accessor      application-input-controller-failure
    :type          (option serious-condition)
    :documentation "A fatal terminal-reader condition awaiting main-thread handling.")
   (failure-backtrace
    :initform      nil
    :accessor      application-input-controller-failure-backtrace
    :type          list
    :documentation "The bounded reader backtrace frames captured with FAILURE."))
  (:documentation
   "Ephemeral terminal input and FIFO submission state for one application run."))

(defmethod initialize-instance :after
    ((controller application-input-controller) &key)
  "Give CONTROLLER its stopped terminal input pump."
  (setf (slot-value controller 'input-pump)
        (make-input-pump
         :name                 "Autolith terminal input"
         :ready-function       (lambda ()
                                 (application-input-controller--input-ready-p controller))
         :step-function        (lambda ()
                                 (application-input-controller--reader-step controller))
         :startable-p-function (lambda ()
                                 (not (application-input-controller-exit-requested-p
                                       controller)))
         :failure-function     (lambda (condition backtrace)
                                 (application-input-controller--record-failure
                                  controller condition backtrace))
         :backtrace-function   #'application-safe-backtrace)))

(defmethod application-input-controller-wake
    ((controller application-input-controller))
  "Wake CONTROLLER's terminal reader for pending presentation work."
  (with-lock-held ((application-input-controller-lock controller))
    (sb-thread:condition-broadcast
     (application-input-controller-condition-variable controller)))
  (input-pump-wake (application-input-controller-input-pump controller))
  nil)

(-> channel-open-command (string) string)
(defun channel-open-command (identifier)
  "Return the shell command that opens the channel named by IDENTIFIER."
  (format nil "autolith open ~A"
          (uiop:escape-shell-token (conversation-identifier-display identifier))))

(-> channel-open-instruction (string) list)
(defun channel-open-instruction (identifier)
  "Return styled advice for opening the channel named by IDENTIFIER."
  (list (terminal-span ':dim "· to open this channel again, run:")
        (terminal-span ':plain (string #\Newline))
        (terminal-span ':code (format nil "  ~A" (channel-open-command identifier)))))

(-> application-input-controller--queues
    (application-input-controller)
    list)
(defun application-input-controller--queues (controller)
  "Return CONTROLLER's mutable input deques in scheduler order."
  (list (application-input-controller-initial-work-items controller)
        (application-input-controller-work-items controller)
        (application-input-controller-steering-items controller)
        (application-input-controller-steering-in-flight-items controller)
        (application-input-controller-pending-apply-items controller)))

(-> application-input-controller--clear-queues-locked (application-input-controller) list)
(defun application-input-controller--clear-queues-locked (controller)
  "Clear CONTROLLER's queues and return removed client jobs.

The caller holds the controller lock and cancels returned jobs outside it."
  (let ((jobs (loop for item in (deque->list (application-input-controller-work-items controller))
                    when (eq (first item) ':client-work)
                      collect (second item))))
    (mapc #'deque-clear (application-input-controller--queues controller))
    jobs))

(-> application-input-controller--prepare-shutdown
    (application-input-controller keyword)
    (values boolean boolean))
(defun application-input-controller--prepare-shutdown (controller reason)
  "Prepare CONTROLLER shutdown and report active and prepared state.

The first value reports whether a model or command turn needs cancellation. The
second reports whether shutdown was prepared."
  (let ((active-p            nil)
        (prepared-p          nil)
        (pending-publication nil)
        (cancelled           nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf active-p (application-input-controller-active-p controller))
      (unless (application-input-controller-stopping-p controller)
        (setf prepared-p t)
        (unless (application-input-controller-exit-reason controller)
          (setf (application-input-controller-exit-reason controller) reason))
        (when active-p
          (setf (application-input-controller-turn-cancellation-p controller) t
                (application-input-controller-turn-cancellation-delivery-pending-p
                 controller)
                t))
        ;; Capture every accepted input before clearing process-local queues so
        ;; an ordinary restart can restore it for this conversation.
        (setf pending-publication
              (application-input-controller--capture-pending-publication-locked
               controller
               (application-input-controller--next-publication-generation-locked
                controller)
               t))
        (setf cancelled (application-input-controller--clear-queues-locked controller))
        (setf (application-input-controller-stopping-p controller) t
              (application-deferred-goal-continuation
               (application-input-controller-application controller)) nil
              (application-input-controller-active-work controller) nil
              (application-input-controller-active-work-identifier controller) nil
              (application-input-controller-pending-snapshot-identifier controller) nil
              (application-input-controller-vault-capture-identifiers controller) nil
              (application-input-controller-steering-promotion-prefix-count controller) 0
              (application-input-controller-follow-up-edit-index controller) nil
              (application-input-controller-follow-up-edit-work controller) nil)
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller))))
    (dolist (work cancelled)
      (job-cancel work :reason ':shutdown))
    (when pending-publication
      (application-input-controller--publish-pending-publication
       controller pending-publication nil))
    (values active-p prepared-p)))

(-> application-input-controller--virtual-work-items
    (application-input-controller)
    list)
(defun application-input-controller--virtual-work-items (controller)
  "Return CONTROLLER's FIFO work including restore and recalled follow-ups.

The caller must hold CONTROLLER's lock."
  (let ((work-items
          (deque-copy (application-input-controller-work-items controller)))
        (index
          (application-input-controller-follow-up-edit-index controller))
        (work
          (application-input-controller-follow-up-edit-work controller)))
    (when (and index work)
      (deque-insert work-items (min index (deque-count work-items)) work))
    (append
     (application-input-controller-vault-restore-work-items controller)
     (deque->list work-items))))

(-> application-input-controller--state
    (application-input-controller &optional keyword)
    t)
(defun application-input-controller--state (controller &optional key)
  "Return detached queue projections and transactional controller metadata.

When KEY is present, return that state value. The caller must hold CONTROLLER's
lock."
  (let* ((queues
           (mapcar #'deque->list
                   (rest (application-input-controller--queues controller))))
         (state
           (list
            :active-work              (application-input-controller-active-work controller)
            :active-work-identifier
            (application-input-controller-active-work-identifier controller)
            :work-items               (first queues)
            :steering-items           (second queues)
            :steering-in-flight-items (third queues)
            :follow-up-edit-index
            (application-input-controller-follow-up-edit-index controller)
            :follow-up-edit-work
            (application-input-controller-follow-up-edit-work controller)
            :snapshot-identifier
            (application-input-controller-pending-snapshot-identifier controller)
            :vault-capture-identifiers
            (application-input-controller-vault-capture-identifiers controller)
            :steering-promotion-prefix-count
            (application-input-controller-steering-promotion-prefix-count controller))))
    (if key (getf state key) state)))

(-> application-input-controller--next-publication-generation-locked
    (application-input-controller)
    (integer 1))
(defun application-input-controller--next-publication-generation-locked (controller)
  "Return CONTROLLER's next external-publication generation.

The caller must hold CONTROLLER's lock."
  (incf (application-input-controller-publication-generation controller)))

(-> application-input-controller--capture-pending-publication-locked
    (application-input-controller (integer 1) boolean)
    (option list))
(defun application-input-controller--capture-pending-publication-locked
    (controller generation sync-vault-p)
  "Return a detached durable pending publication for CONTROLLER.

The caller must hold CONTROLLER's lock."
  (block nil
    (unless (application-input-controller-pending-persistence-enabled-p controller)
      (return nil))
    (let* ((application (application-input-controller-application controller))
           (configuration
             (and (slot-boundp application 'configuration)
                  (application-configuration application)))
           (conversation
             (and (slot-boundp application 'conversation)
                  (application-conversation application))))
      (unless (and (typep configuration 'configuration)
                   (typep conversation 'conversation))
        (return nil))
      (let* ((conversation-identifier (conversation-identifier conversation))
             (pathname
               (configuration-pending-inputs-path
                configuration (conversation-pathname conversation)))
             (old-snapshot-identifier
               (application-input-controller-pending-snapshot-identifier controller))
             (old-vault-capture-identifiers
               (application-input-controller-vault-capture-identifiers controller))
             (old-steering-promotion-prefix-count
               (application-input-controller-steering-promotion-prefix-count controller))
             (restore-work
               (application-input-controller-vault-restore-work-items controller))
             (state                   (application-input-controller--state controller))
             (work                    (application-input-controller--virtual-work-items controller))
             (pending-p
               (not
                (null
                 (or (getf state :active-work)
                     (getf state :steering-in-flight-items)
                     (getf state :steering-items)
                     work)))))
        (setf (getf state :work-items) work
              (getf state :vault-capture-identifiers)
              (remove-duplicates
               (append
                old-vault-capture-identifiers
                (application-input-controller-vault-restore-capture-identifiers
                 controller))
               :test #'string=)
              (getf state :steering-promotion-prefix-count)
              (+ old-steering-promotion-prefix-count (length restore-work)))
        (if pending-p
            (let ((snapshot-identifier
                    (or (getf state :snapshot-identifier)
                        (setf
                         (application-input-controller-pending-snapshot-identifier
                          controller)
                         (make-identifier)))))
              (setf (getf state :snapshot-identifier) snapshot-identifier)
              (let* ((form
                       (input-state->form
                        state conversation-identifier))
                     (detached-state
                       (input-state-from-form
                        form conversation-identifier)))
                (list :generation   generation
                      :application  application
                      :pathname     pathname
                      :pending-p    t
                      :form         form
                      :state        detached-state
                      :sync-vault-p sync-vault-p
                      :create-vault-p
                      (application-input-controller-live-vault-sync-p controller))))
            (list :generation   generation
                  :application  application
                  :pathname     pathname
                  :pending-p    nil
                  :old-snapshot-identifier
                  (and old-snapshot-identifier
                       (copy-seq old-snapshot-identifier))
                  :old-vault-capture-identifiers
                  (mapcar #'copy-seq old-vault-capture-identifiers)
                  :old-steering-promotion-prefix-count
                  old-steering-promotion-prefix-count
                  :sync-vault-p sync-vault-p))))))

(-> application-input-controller--commit-empty-publication-locked
    (application-input-controller list)
    null)
(defun application-input-controller--commit-empty-publication-locked
    (controller publication)
  "Clear pending metadata after an empty PUBLICATION succeeds.

The caller must hold CONTROLLER's publication lock."
  (with-lock-held ((application-input-controller-lock controller))
    (let* ((state (application-input-controller--state controller))
           (work  (application-input-controller--virtual-work-items controller))
           (pending-p
             (or (getf state :active-work)
                 (getf state :steering-in-flight-items)
                 (getf state :steering-items)
                 work)))
      (when (and
             (null pending-p)
             (= (getf publication :generation)
                (application-input-controller-publication-generation controller))
             (equal (getf publication :old-snapshot-identifier)
                    (application-input-controller-pending-snapshot-identifier controller))
             (equal (getf publication :old-vault-capture-identifiers)
                    (application-input-controller-vault-capture-identifiers controller))
             (= (getf publication :old-steering-promotion-prefix-count)
                (application-input-controller-steering-promotion-prefix-count controller)))
        (setf
         (application-input-controller-pending-snapshot-identifier controller) nil
         (application-input-controller-vault-capture-identifiers controller) nil
         (application-input-controller-steering-promotion-prefix-count controller) 0))))
  nil)

(-> application-input-controller--publish-pending-publication-locked
    (application-input-controller list boolean)
    boolean)
(defun application-input-controller--publish-pending-publication-locked
    (controller publication error-p)
  "Publish detached pending PUBLICATION while holding CONTROLLER's publication lock."
  (let ((generation (getf publication :generation)))
    (when (<= generation
              (application-input-controller-published-pending-generation controller))
      (let ((durable-p
              (<= generation
                  (application-input-controller-durable-pending-generation controller))))
        (when (and error-p (not durable-p))
          (error 'configuration-error
                 :message (format nil "Pending input generation ~D has not been durably published."
                                  generation)))
        (return-from application-input-controller--publish-pending-publication-locked
          durable-p)))
    (setf (application-input-controller-published-pending-generation controller)
          generation)
    (handler-case
        (let ((application (getf publication :application))
              (pathname    (getf publication :pathname)))
          (if (getf publication :pending-p)
              (progn
                (ensure-directories-exist pathname)
                (snapshot-write pathname (getf publication :form) :mode #o600)
                (when (getf publication :sync-vault-p)
                  (application-recovery-input-vault--sync-pending-locked
                   application
                   (getf publication :state)
                   :create-p (getf publication :create-vault-p))))
              (progn
                (when (probe-file pathname)
                  (delete-file pathname))
                (when (and (getf publication :sync-vault-p)
                           (non-empty-string-p
                            (getf publication :old-snapshot-identifier)))
                  (application-recovery-input-vault--replace-capture-locked
                   application
                   (getf publication :old-snapshot-identifier)
                   nil))
                (application-input-controller--commit-empty-publication-locked
                 controller publication)))
          (setf (application-input-controller-durable-pending-generation controller)
                generation)
          t)
      (error (condition)
        (when error-p
          (error condition))
        nil))))

(-> application-input-controller--publish-pending-publication
    (application-input-controller list boolean)
    boolean)
(defun application-input-controller--publish-pending-publication
    (controller publication error-p)
  "Serialize and publish detached pending PUBLICATION for CONTROLLER."
  (with-lock-held ((application-input-controller-publication-lock controller))
    (application-input-controller--publish-pending-publication-locked
     controller publication error-p)))

(-> application-input-controller--persist-pending
    (application-input-controller &key (:error-p boolean) (:sync-vault-p boolean))
    boolean)
(defun application-input-controller--persist-pending
    (controller &key (error-p nil) (sync-vault-p t))
  "Atomically publish CONTROLLER's accepted but unprocessed input."
  (handler-case
      (let ((publication
              (with-lock-held ((application-input-controller-lock controller))
                (application-input-controller--capture-pending-publication-locked
                 controller
                 (application-input-controller--next-publication-generation-locked
                  controller)
                 sync-vault-p))))
        (and publication
             (application-input-controller--publish-pending-publication
              controller publication error-p)))
    (error (condition)
      (when error-p
        (error condition))
      nil)))

(-> application-input-controller--load-pending
    (application-input-controller)
    null)
(defun application-input-controller--load-pending (controller)
  "Restore accepted pending input for CONTROLLER's conversation."
  (let* ((application (application-input-controller-application controller))
         (configuration
           (and (slot-boundp application 'configuration)
                (application-configuration application)))
         (conversation
           (and (slot-boundp application 'conversation)
                (application-conversation application))))
    (when (and (typep configuration 'configuration)
               (typep conversation 'conversation))
      (let* ((pathname
               (configuration-pending-inputs-path
                configuration (conversation-pathname conversation)))
             (legacy-pathname
               (configuration-legacy-pending-inputs-path configuration))
             (source-pathname
               (cond
                 ((probe-file pathname)
                  pathname)
                 ((probe-file legacy-pathname)
                  legacy-pathname)
                 (t
                  nil))))
        (when source-pathname
          (handler-case
              (multiple-value-bind (form complete-p)
                  (snapshot-read source-pathname)
                (let* ((state
                         (and complete-p
                              (input-state-from-form
                               form (conversation-identifier conversation))))
                       (filtered-state
                         (and state
                              (input-state-filter-persisted
                               state conversation))))
                  (when filtered-state
                    (let* ((active-work         (getf filtered-state :active-work))
                           (steering
                             (application-input-controller-steering-items
                              controller))
                           (work
                             (application-input-controller-work-items controller))
                           (pending-publication nil))
                      (with-lock-held
                          ((application-input-controller-lock controller))
                        (setf
                         (application-input-controller-pending-snapshot-identifier
                          controller)
                         (getf filtered-state :snapshot-identifier)
                         (application-input-controller-vault-capture-identifiers
                          controller)
                         (getf filtered-state :vault-capture-identifiers)
                         (application-input-controller-steering-promotion-prefix-count
                          controller)
                         (getf filtered-state :steering-promotion-prefix-count)
                         (application-input-controller-active-work controller) nil
                         (application-input-controller-active-work-identifier
                          controller)
                         nil)
                        (deque-clear
                         (application-input-controller-steering-in-flight-items
                          controller))
                        (deque-prepend
                         steering (getf filtered-state :steering-items))
                        (deque-prepend
                         steering
                         (getf filtered-state :steering-in-flight-items)
                         :key #'agent-steering-input-content)
                        (deque-prepend work (getf filtered-state :work-items))
                        (when active-work
                          (deque-push-front work active-work))
                        (setf pending-publication
                              (application-input-controller--capture-pending-publication-locked
                               controller
                               (application-input-controller--next-publication-generation-locked
                                controller)
                               nil)))
                      (when pending-publication
                        (application-input-controller--publish-pending-publication
                         controller pending-publication nil))
                      (when (equal source-pathname legacy-pathname)
                        (when (probe-file pathname)
                          (delete-file legacy-pathname)))))))
            (error ()
              nil)))))
   nil))

(-> application-input-controller--publish-counts
    (application-input-controller)
    null)
(defun application-input-controller--publish-counts (controller)
  "Publish detached pending input previews and durable state in generation order."
  (let ((generation          nil)
        (ui                  nil)
        (steering-inputs     nil)
        (queued-inputs       nil)
        (pending-publication nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf generation
            (application-input-controller--next-publication-generation-locked
             controller)
            ui
            (application-ui
             (application-input-controller-application controller))
            steering-inputs
            (mapcar (lambda (input)
                      (copy-seq (user-message-input-text input)))
                    (deque->list
                     (application-input-controller-steering-items controller)))
            queued-inputs
            (loop for work in
                  (deque->list
                   (application-input-controller-work-items controller))
                  for input = (second work)
                  when (typep input '(or string user-message-input))
                    collect (copy-seq (user-message-input-text input)))
            pending-publication
            (unless (application-input-controller-stopping-p controller)
              (application-input-controller--capture-pending-publication-locked
               controller generation t))))
    (with-lock-held ((application-input-controller-publication-lock controller))
      (when (> generation
               (application-input-controller-published-ui-generation controller))
        (setf (application-input-controller-published-ui-generation controller)
              generation)
        (when ui
          (terminal-ui-set-pending-inputs ui steering-inputs queued-inputs)))
     (when pending-publication
       (application-input-controller--publish-pending-publication-locked
        controller pending-publication nil))))
  (application-input-controller--note-turn controller)
  ;; Queued Lisp work has no text preview, but still changes runtime queue counts.
  (client-hub-note-change (application-client-hub (application-input-controller-application controller)))
  nil)

(-> application-input-controller-turn-state (application-input-controller) list)
(defun application-input-controller-turn-state (controller)
  "Return whether CONTROLLER works, cancels its turn, or holds a recalled follow-up, as :TURN fields.

The slots are read without the controller's lock, so presenting a change
never waits for work that holds it."
  (list :active-p     (not (null (application-input-controller-active-p controller)))
        :cancelling-p (not (null (application-input-controller-turn-cancellation-p controller)))
        :editing-p    (not (null (application-input-controller-follow-up-edit-index controller)))))

(-> application-input-controller--note-turn (application-input-controller) null)
(defun application-input-controller--note-turn (controller)
  "Tell a hosted channel's clients whether CONTROLLER still works, when that changed."
  (when-let ((presentation (application-presentation
                            (application-input-controller-application controller))))
    (channel-presentation-note-turn presentation))
  nil)

(-> application-input-controller-turn-active-p
    (application-input-controller)
    boolean)
(defun application-input-controller-turn-active-p (controller)
  "Return true when CONTROLLER's main thread is processing one work item."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-active-p controller)))))

(-> application-input-controller-busy-p
    (application-input-controller)
    boolean)
(defun application-input-controller-busy-p (controller)
  "Return true when CONTROLLER has active or pending application work."
  (with-lock-held ((application-input-controller-lock controller))
    (application-input-controller--busy-locked-p controller)))

(-> application-input-controller--busy-locked-p (application-input-controller) boolean)
(defun application-input-controller--busy-locked-p (controller)
  "Return whether CONTROLLER has active or pending work under the held controller lock."
  (not (null (or (application-input-controller-active-p controller)
                 (find-if-not #'deque-empty-p (application-input-controller--queues controller))
                 (application-input-controller-follow-up-edit-work controller)))))

(-> application-input-controller--follow-up-editing-p
    (application-input-controller)
    boolean)
(defun application-input-controller--follow-up-editing-p (controller)
  "Return true when CONTROLLER has recalled one queued follow-up into the draft."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-follow-up-edit-index controller)))))

(-> application-input-controller--remove-recalled-prefix-slot-locked
    (application-input-controller (integer 0))
    null)
(defun application-input-controller--remove-recalled-prefix-slot-locked
    (controller index)
  "Remove recalled virtual INDEX from CONTROLLER's promoted FIFO prefix."
  (when (< index
           (application-input-controller-steering-promotion-prefix-count
            controller))
    (decf
     (application-input-controller-steering-promotion-prefix-count controller)))
  nil)

(-> application-input-controller--clear-follow-up-edit
    (application-input-controller)
    null)
(defun application-input-controller--clear-follow-up-edit (controller)
  "Discard CONTROLLER's recalled follow-up without changing its draft."
  (with-lock-held ((application-input-controller-lock controller))
    (let ((index
            (application-input-controller-follow-up-edit-index controller)))
      (when index
        (application-input-controller--remove-recalled-prefix-slot-locked
         controller index))
      (setf (application-input-controller-follow-up-edit-index controller) nil
            (application-input-controller-follow-up-edit-work controller) nil)
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller))))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--interrupt-main
    (application-input-controller condition)
    null)
(defun application-input-controller--interrupt-main (controller condition)
  "Signal CONDITION on CONTROLLER's main thread unless already running there."
  (unless (application-assignment-cancel
           (application-input-controller-application controller) :reason ':shutdown)
    (let ((thread (application-input-controller-main-thread controller)))
      (unless (eq thread (current-thread))
        (when (thread-alive-p thread)
          (interrupt-thread thread (lambda () (error condition)))))))
  nil)

(-> application-input-controller--consume-turn-cancellation-delivery-p
    (application-input-controller)
    boolean)
(defun application-input-controller--consume-turn-cancellation-delivery-p
    (controller)
  "Atomically consume and report CONTROLLER's pending cancellation delivery."
  (eq (sb-ext:compare-and-swap
       (slot-value controller 'turn-cancellation-delivery-pending-p)
       t
       nil)
      t))

(-> application-input-controller--interrupt-main-for-turn-cancellation
    (application-input-controller)
    null)
(defun application-input-controller--interrupt-main-for-turn-cancellation
    (controller)
  "Promptly deliver CONTROLLER's pending turn cancellation to its main thread.

A thread outside its work handler keeps the request pending; the handler
consumes it when the work enters."
  (if (application-assignment-cancel
       (application-input-controller-application controller) :reason ':cancelled)
      (application-input-controller--consume-turn-cancellation-delivery-p controller)
      (let ((thread (application-input-controller-main-thread controller)))
        (unless (eq thread (current-thread))
          (when (thread-alive-p thread)
            (interrupt-thread
             thread
             (lambda ()
               (when (and *application-turn-cancellation-deliverable-p*
                          (application-input-controller--consume-turn-cancellation-delivery-p
                           controller))
                 (error (make-condition 'application-turn-cancelled)))))))))
  nil)

(-> application-input-controller--record-failure
    (application-input-controller serious-condition list)
    null)
(defun application-input-controller--record-failure
    (controller condition backtrace)
  "Record reader CONDITION, preserve pending input, and wake the main thread."
  (let ((active-p            nil)
        (pending-publication nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-failure controller)
        (setf (application-input-controller-failure controller) condition
              (application-input-controller-failure-backtrace controller) backtrace
              (application-input-controller-stopping-p controller) t
              pending-publication
              (handler-case
                  (application-input-controller--capture-pending-publication-locked
                   controller
                   (application-input-controller--next-publication-generation-locked controller)
                   t)
                (error () nil))))
      (setf active-p (application-input-controller-active-p controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (when pending-publication
      ;; Teardown may clear the queues as soon as it sees the failure. Publish
      ;; the detached state captured before waking it, never a later empty queue.
      (handler-case
          (application-input-controller--publish-pending-publication
           controller pending-publication nil)
        (error () nil)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition
            'application-input-failed
            :original-condition condition
            :backtrace          backtrace))
        (error ()
          nil))))
  nil)

(-> application-input-controller--enqueue
    (application-input-controller keyword (or string user-message-input))
    boolean)
(defun application-input-controller--enqueue (controller kind input)
  "Append one work item of KIND carrying INPUT and report acceptance."
  (let ((queued-p  nil)
        (resumed-p nil))
    (with-lock-held ((application-input-controller-lock controller))
        (unless (application-input-controller-stopping-p controller)
         (setf resumed-p
               (application-input-controller-queued-work-paused-p controller)
               (application-input-controller-queued-work-paused-p controller) nil
               queued-p t
               (application-input-controller-live-vault-sync-p controller) t)
         (deque-push-back
          (application-input-controller-work-items controller)
          (append (list kind (user-message-input-copy
                              (if (eq kind ':message) input (user-message-input-text input))))
                  (when (and (not (eq kind ':message)) (user-message-input-submission input))
                    (list :submission (user-message-input-submission input)))))
         (sb-thread:condition-broadcast
          (application-input-controller-condition-variable controller))))
    (when resumed-p
      (terminal-ui-set-notice
       (application-ui (application-input-controller-application controller))
       nil))
    (application-input-controller--publish-counts controller)
    queued-p))

(-> application-input-controller-enqueue-operation-request
    (application-input-controller (or string user-message-input) list
     &key (:pending-input-identifier (option non-empty-string)))
    boolean)
(defun application-input-controller-enqueue-operation-request
    (controller text request &key pending-input-identifier)
  "Queue one model turn for operation-generated TEXT and report acceptance.

REQUEST is the portable metadata the transcript shows in place of TEXT. The
turn runs as automatic work, like a job completion, so it never enters the
user's input history, pending follow-ups, or recovery vault. With
PENDING-INPUT-IDENTIFIER, the turn records that receipt and is skipped when
the conversation already holds it."
  (let ((queued-p nil))
    (with-lock-held ((application-input-controller-lock controller))
        (unless (application-input-controller-stopping-p controller)
         (deque-push-back
          (application-input-controller-work-items controller)
          (list ':operation-request
                 (list :text                    (user-message-input-copy text)
                      :request                  (copy-tree request)
                      :pending-input-identifier (and pending-input-identifier
                                                     (copy-seq pending-input-identifier)))))
         (setf queued-p t)
         (sb-thread:condition-broadcast
          (application-input-controller-condition-variable controller))))
    queued-p))

(-> application-input-controller-enqueue-work (application-input-controller list) boolean)
(defun application-input-controller-enqueue-work (controller work)
  "Queue the work item WORK, such as a client's (:CLIENT-WORK WORK), and report acceptance.

Such work is not input: it is never persisted as a follow-up, recorded in
input history, or kept in the recovery vault."
  (let ((queued-p nil))
    (with-lock-held ((application-input-controller-lock controller))
        (unless (application-input-controller-stopping-p controller)
         (deque-push-back (application-input-controller-work-items controller) work)
         (setf queued-p t)
         (sb-thread:condition-broadcast
          (application-input-controller-condition-variable controller))))
    queued-p))

(-> application-input-controller--queue-input
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--queue-input (controller input)
  "Queue INPUT, restoring a recalled follow-up to its virtual FIFO position."
  (let ((work      (input-work-from-input input))
        (queued-p  nil)
        (resumed-p nil))
    (when work
      (with-lock-held ((application-input-controller-lock controller))
          (unless (application-input-controller-stopping-p controller)
           (let ((index
                   (application-input-controller-follow-up-edit-index controller))
                 (items (application-input-controller-work-items controller)))
             (setf resumed-p
                   (application-input-controller-queued-work-paused-p controller)
                   (application-input-controller-queued-work-paused-p controller) nil
                   (application-input-controller-follow-up-edit-index controller) nil
                   (application-input-controller-follow-up-edit-work controller) nil
                   queued-p t
                   (application-input-controller-live-vault-sync-p controller) t)
             (if index
                 (deque-insert items (min index (deque-count items)) work)
                 (deque-push-back items work)))
           (sb-thread:condition-broadcast
            (application-input-controller-condition-variable controller))))
      (when resumed-p
        (terminal-ui-set-notice
         (application-ui (application-input-controller-application controller))
         nil))
      (application-input-controller--publish-counts controller))
    queued-p))

(-> application-input-controller--prompt-storage-ready-p
    (application-input-controller)
    boolean)
(defun application-input-controller--prompt-storage-ready-p (controller)
  "Return whether a primary prompt can be durably accepted by CONTROLLER."
  (or (application-input-controller-pending-persistence-enabled-p controller)
      (progn
        (application-present
         (application-input-controller-application controller)
         (application-failure-line
          "recovered input storage is unavailable; (vault) inspects it and (vault-discard) discards the preserved input"))
        nil)))

(-> application-input-controller--admit-primary-prompt-locked
    (application-input-controller (or string user-message-input)
     &key (:prefer-steering-p boolean)
          (:queue-index (option (integer 0))))
    (option (member :queued :steering)))
(defun application-input-controller--admit-primary-prompt-locked
    (controller input &key (prefer-steering-p t) queue-index)
  "Admit INPUT while CONTROLLER's lock is held and return its delivery mode.

QUEUE-INDEX restores recalled work to its virtual FIFO position when INPUT does
not steer the active primary message."
    (when (application-input-controller-stopping-p controller)
      (return-from application-input-controller--admit-primary-prompt-locked nil))
  (let* ((copied-input (user-message-input-copy input))
         (queued-work  (list ':message copied-input))
         (work         (application-input-controller-work-items controller)))
    (cond
      ((and prefer-steering-p
            (or (eq (application-input-controller-active-work-kind controller)
                    ':message)
                (application-input-controller-primary-steering-p controller)))
       (deque-push-back
        (application-input-controller-steering-items controller)
        copied-input)
       ':steering)
      (queue-index
       (deque-insert work
                     (min queue-index (deque-count work))
                     queued-work)
       ':queued)
      ((and prefer-steering-p
            (not (application-input-controller-active-p controller)))
       (let ((index
               (min
                (application-input-controller-steering-promotion-prefix-count
                 controller)
                (deque-count work))))
         (deque-insert work index queued-work)
         (setf (application-input-controller-steering-promotion-prefix-count
                controller)
               (1+ index))
         (when (application-input-controller-follow-up-edit-index controller)
           (incf (application-input-controller-follow-up-edit-index controller))))
       ':queued)
      (t
       (deque-push-back work queued-work)
       ':queued))))

(-> application-input-controller--agentless-refusal (application) list)
(defun application-input-controller--agentless-refusal (application)
  "Return the failure line refusing a prompt in APPLICATION's channel, where no agent resides."
  (flet ((failure (text)
           (terminal-span ':failure text)))
    (if (application-world-inbox-p application)
        (append (list (failure "! no agent responds to your inbox; try "))
                (application-lisp-code-spans "(tell \"OTHER-CHANNEL\" \"TEXT\")")
                (list (failure " or "))
                (application-lisp-code-spans "(open \"ID\")"))
        (append (list (failure "! this channel has no agent; try "))
                (application-lisp-code-spans "(open \"ID\")")))))

(-> application-input-controller-submit-primary-prompt
    (application-input-controller (or string user-message-input)
     &key (:prefer-steering-p boolean))
    (values boolean (member :queued :rejected :steering :no-agent)))
(defun application-input-controller-submit-primary-prompt
    (controller input &key (prefer-steering-p t))
  "Submit INPUT to the primary agent and report acceptance and delivery mode.

When steering is preferred, an active message receives INPUT through its
steering mailbox. Input submitted after that message ended runs before older
follow-ups. Input submitted during local Lisp or other non-message work joins
the ordinary FIFO queue."
  (unless (application-input-controller--prompt-storage-ready-p controller)
    (return-from application-input-controller-submit-primary-prompt
      (values nil ':rejected)))
  (let ((application (application-input-controller-application controller)))
    (unless (application-world-agent-present-p application)
      (application-present application (application-input-controller--agentless-refusal application))
      (return-from application-input-controller-submit-primary-prompt
        (values nil ':no-agent))))
  (let ((delivery  nil)
        (resumed-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf delivery
            (application-input-controller--admit-primary-prompt-locked
             controller input :prefer-steering-p prefer-steering-p))
      (when delivery
        (setf resumed-p
              (application-input-controller-queued-work-paused-p controller)
              (application-input-controller-queued-work-paused-p controller) nil
              (application-input-controller-live-vault-sync-p controller) t)
       (sb-thread:condition-broadcast
        (application-input-controller-condition-variable controller))))
    (when resumed-p
      (when-let ((ui (application-ui
                      (application-input-controller-application controller))))
        (terminal-ui-set-notice ui nil)))
    (application-input-controller--publish-counts controller)
    ;; The person's own submission, and only that, is shared with the
    ;; participants elsewhere it reaches: a replayed or relayed message
    ;; never enters here.
    (when delivery
      (application-world-share (application-input-controller-application controller) input))
    (values (not (null delivery)) (or delivery ':rejected))))

(-> application-input-controller--take-steering
    (application-input-controller)
    list)
(defun application-input-controller--take-steering (controller)
  "Move queued steering into durable in-flight entries and return them."
  (let ((entries nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-stopping-p controller)
        (multiple-value-bind (ignored-target ignored-evicted moved)
            (deque-move-all
             (application-input-controller-steering-items controller)
             (application-input-controller-steering-in-flight-items controller)
             :key #'application-input-controller--steering-entry)
          (declare (ignore ignored-target ignored-evicted))
          (setf entries (coerce moved 'list)))))
    (when entries
      ;; The old snapshot still contains queued steering if this atomic
      ;; replacement fails, while the new snapshot names every in-flight item.
      (application-input-controller--persist-pending controller))
    (application-input-controller--publish-counts controller)
    entries))

(-> application-input-controller--take-promoted-steering (application-input-controller) list)
(defun application-input-controller--take-promoted-steering (controller)
  "Take consecutive promoted messages for the first request of the active turn."
  (let ((entries nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-promoted-steering-p controller)
                 (not (application-input-controller-stopping-p controller))
                 (not (application-input-controller-turn-cancellation-p controller)))
        (let ((work-items (application-input-controller-work-items controller))
              (in-flight  (application-input-controller-steering-in-flight-items controller)))
          (sb-sys:without-interrupts
            (loop while (and (plusp (application-input-controller-steering-promotion-prefix-count controller))
                             (not (eql (application-input-controller-follow-up-edit-index controller) 0))
                             (not (deque-empty-p work-items)))
                  do (let ((work (deque-ref work-items 0)))
                       (unless (eq (first work) ':message)
                         (return))
                       (let ((entry (application-input-controller--steering-entry (second work))))
                         (deque-push-back in-flight entry)
                         (deque-pop-front work-items)
                         (decf (application-input-controller-steering-promotion-prefix-count controller))
                         (when (application-input-controller-follow-up-edit-index controller)
                           (decf (application-input-controller-follow-up-edit-index controller)))
                         (push entry entries))))))))
    (when entries
      (application-input-controller--persist-pending controller)
      (application-input-controller--publish-counts controller))
    (nreverse entries)))

(-> application-input-controller--take-primary-steering (application-input-controller) list)
(defun application-input-controller--take-primary-steering (controller)
  "Take durable terminal steering before the channel's world deliveries."
  (append (application-input-controller--take-steering controller)
          (application-world-steering-take (application-input-controller-application controller))))

(-> application-input-controller--acknowledge-primary-steering
    (application-input-controller non-empty-string)
    boolean)
(defun application-input-controller--acknowledge-primary-steering (controller identifier)
  "Settle a durable world receipt, or retire the terminal entry it does not name."
  (or (application-world-steering-ack (application-input-controller-application controller) identifier)
      (application-input-controller--acknowledge-steering controller identifier)))

(-> application-input-controller-call-with-primary-steering
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-primary-steering
    (controller function)
  "Call FUNCTION with terminal prose steering the running primary turn.

FUNCTION receives a take function moving queued steering into durable
in-flight entries and an acknowledge function retiring one delivered
entry. While FUNCTION runs, submitted prose steers even though the
active work item is not a message, so command-driven turns such as goal
work receive steering as soon as possible instead of as follow-ups."
  (with-lock-held ((application-input-controller-lock controller))
    (setf (application-input-controller-primary-steering-p controller) t))
  (unwind-protect
       (funcall function
                (lambda ()
                  (application-input-controller--take-primary-steering controller))
                (lambda (identifier)
                  (application-input-controller--acknowledge-primary-steering controller identifier)))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-primary-steering-p controller)
            nil))))

(-> application-input-controller--acknowledge-active-work
    (application-input-controller non-empty-string)
    boolean)
(defun application-input-controller--acknowledge-active-work
    (controller identifier)
  "Forget active work IDENTIFIER after its user message is durable."
  (let ((acknowledged-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-work controller)
                 (string=
                  identifier
                  (or (application-input-controller-active-work-identifier
                       controller)
                      "")))
        (setf (application-input-controller-active-work controller) nil
              (application-input-controller-active-work-identifier controller) nil
              acknowledged-p t)))
    (when acknowledged-p
      (application-input-controller--persist-pending controller))
    acknowledged-p))

(-> application-input-controller--acknowledge-steering
    (application-input-controller non-empty-string)
    boolean)
(defun application-input-controller--acknowledge-steering (controller identifier)
  "Retire steering IDENTIFIER after its message is persisted."
  (let ((acknowledged-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf acknowledged-p
            (nth-value 1
                       (deque-delete identifier
                                     (application-input-controller-steering-in-flight-items controller)
                                     :key #'agent-steering-input-identifier :test #'string=))))
    (when acknowledged-p
      (application-input-controller--persist-pending controller))
    acknowledged-p))

(-> application-input-controller--request-idle-exit (application-input-controller) boolean)
(defun application-input-controller--request-idle-exit (controller)
  "Commit an idle CONTROLLER to exit under the controller lock."
  (with-lock-held ((application-input-controller-lock controller))
    (application-input-controller--request-idle-exit-locked controller)))

(-> application-input-controller--request-idle-exit-locked (application-input-controller) boolean)
(defun application-input-controller--request-idle-exit-locked (controller)
  "Commit an idle CONTROLLER to exit with the controller lock already held."
  (cond
    ((or (application-input-controller-stopping-p controller)
         (application-input-controller--busy-locked-p controller))
     nil)
    (t
     (setf (application-input-controller-exit-reason controller) ':world-terminal-kill
           (application-input-controller-stopping-p controller) t)
     (sb-thread:condition-broadcast (application-input-controller-condition-variable controller))
     t)))

(-> application-input-controller--request-exit
    (application-input-controller keyword)
    null)
(defun application-input-controller--request-exit (controller reason)
  "Stop CONTROLLER for REASON, discarding work and cancelling an active turn."
  (multiple-value-bind (active-p prepared-p)
      (application-input-controller--prepare-shutdown controller reason)
    (when (and active-p prepared-p)
      (handler-case
          (application-input-controller--interrupt-main-for-turn-cancellation
           controller)
        (error ()
          nil))))
  nil)

(-> application-input-controller--turn-cancellation-active-p
    (application-input-controller)
    boolean)
(defun application-input-controller--turn-cancellation-active-p (controller)
  "Return true while CONTROLLER is still finishing active-turn cancellation."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-turn-cancellation-p controller)))))

(-> application-input-controller--request-active-turn-cancellation
    (application-input-controller
     &key (:pause-queued-work-p boolean))
    boolean)
(defun application-input-controller--request-active-turn-cancellation
    (controller &key pause-queued-work-p)
  "Atomically request cancellation only for CONTROLLER's current active turn.

PAUSE-QUEUED-WORK-P holds follow-ups after cancellation until explicit new input
resumes them. Unconsumed steering is promoted in FIFO order after the turn."
  (let ((accepted-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (not (application-input-controller-stopping-p controller))
                 (not
                  (application-input-controller-turn-cancellation-p controller)))
        (setf (application-input-controller-turn-cancellation-p controller) t
              (application-deferred-goal-continuation
               (application-input-controller-application controller)) nil
              (application-input-controller-turn-cancellation-delivery-pending-p
               controller)
              t
              (application-input-controller-queued-work-paused-p controller)
              pause-queued-work-p
              accepted-p t)))
    (when accepted-p
      (let ((application
              (application-input-controller-application controller)))
        (when (slot-boundp application 'ui)
          (handler-case
              (terminal-ui-set-notice
               (application-ui application)
               "interrupting request"
               :duration-seconds
               *application-interrupted-queue-notice-seconds*)
            (error ()
              nil))))
      (handler-case
          (application-input-controller--interrupt-main-for-turn-cancellation
           controller)
        (error ()
          nil)))
    accepted-p))

(-> application-input-controller--present-scheduled-lisp
    (application-input-controller string)
    null)
(defun application-input-controller--present-scheduled-lisp (controller source)
  "Present the active-turn scheduling result for explicit local Lisp SOURCE."
  (application-present
   (application-input-controller-application controller)
   (list
    (terminal-span ':hint
                   "· local evaluation scheduled until the current response finishes")
    (terminal-span ':plain (string #\Newline))
    (terminal-span ':dim
                   (format nil "  ~A"
                           (text-cell-prefix
                            (sanitize-text source :single-line-p t)
                            72)))))
  nil)

(-> application-input-controller--schedule-lisp
    (application-input-controller (or string user-message-input) &key (:interactive-p boolean))
    boolean)
(defun application-input-controller--schedule-lisp (controller source &key (interactive-p t))
  "Queue local Lisp SOURCE to run at the first idle opportunity."
  (when (application-input-controller--enqueue controller (if interactive-p ':lisp ':native-lisp) source)
    (application-input-controller--present-scheduled-lisp controller (user-message-input-text source))
    t))

(-> application-input-controller--steering-entry
    ((or string user-message-input)) agent-steering-input)
(defun application-input-controller--steering-entry (input)
  "Assign an identifier to steering INPUT before delivery."
  (agent-steering-input-create :identifier (make-identifier) :content input))

(-> application-input-controller--enqueue-apply
    (application-input-controller list) boolean)
(defun application-input-controller--enqueue-apply (controller work)
  "Queue WORK at the next safe provider boundary, ahead of follow-ups."
  (with-lock-held ((application-input-controller-lock controller))
    (when (application-input-controller-stopping-p controller)
      (return-from application-input-controller--enqueue-apply nil))
    (deque-push-back (application-input-controller-pending-apply-items controller) work)
    (sb-thread:condition-broadcast (application-input-controller-condition-variable controller)))
  (application-present
   (application-input-controller-application controller)
   (list
    (terminal-span ':hint
                   "· command applies at the next safe point in the active response")
    (terminal-span ':plain (string #\Newline))
    (terminal-span ':dim
                   (format nil "  ~A"
                           (text-cell-prefix
                             (sanitize-text (user-message-input-text (second work)) :single-line-p t)
                             72)))))
  t)

(-> application-input-controller--schedule-apply
    (application-input-controller (or string user-message-input)
     &optional (option application-command-invocation)) boolean)
(defun application-input-controller--schedule-apply (controller input &optional invocation)
  "Schedule a noninteractive command with its optional decoded INVOCATION."
  (application-input-controller--enqueue-apply
   controller (list* ':native-lisp (user-message-input-copy input) (when invocation (list invocation)))))

(-> application-input-controller--schedule-interactive-apply
    (application-input-controller (or string user-message-input)) boolean)
(defun application-input-controller--schedule-interactive-apply (controller input)
  "Give an explicit terminal settings picker the same boundary as a setting value."
  (application-input-controller--enqueue-apply
   controller (list ':lisp (user-message-input-copy input))))

(-> application-input-controller--take-pending-applies
    (application-input-controller)
    list)
(defun application-input-controller--take-pending-applies (controller)
  "Drain and return CONTROLLER's queued apply-at-boundary command inputs."
  (with-lock-held ((application-input-controller-lock controller))
    (let ((items   (application-input-controller-pending-apply-items controller))
          (drained nil))
      (loop until (deque-empty-p items)
            do (push (deque-pop-front items) drained))
      (nreverse drained))))

(-> application-input-controller--apply-pending-commands
    (application-input-controller &key (:agent t))
    null)
(defun application-input-controller--apply-pending-commands (controller &key agent)
  "Apply queued command inputs, then let running AGENT adopt replaced runtime.

Only terminal picker work runs interactively. AGENT, when supplied, is the loop
still executing the current turn; it adopts the resulting runtime before the
next provider request."
  (let ((items       (application-input-controller--take-pending-applies controller))
        (application (application-input-controller-application controller)))
    (when items
      (dolist (work items)
        (destructuring-bind (kind input &optional invocation) work
          (application-input-controller--call-submitted-input
           controller input
           (lambda ()
             (if invocation
                 (application-command-execute
                  (application-command-invocation-command invocation) application invocation)
                 (application-run-lisp-input application (user-message-input-text input)
                                             :interactive-p (eq kind ':lisp)))))))
      (when agent
        (application--agent-adopt-runtime application agent))))
  nil)

(-> application-input-controller--lisp-active-turn-action
    (application-input-controller string)
    (member :apply :apply-interactive :execute :hold))
(defun application-input-controller--lisp-active-turn-action (controller source)
  "Return SOURCE's admission action when CONTROLLER has an active turn."
  (if (application-input-controller-turn-active-p controller)
      (application-operation-source-active-turn-action
       (application-input-controller-application controller) source)
      ':hold))

(-> application-input-controller--prompt-result-failure-p (t) boolean)
(defun application-input-controller--prompt-result-failure-p (result)
  "Return true when RESULT denotes a failed user interaction."
  (not (null (member result '(:aborted :failed) :test #'eq))))

(-> application-input-controller--open-prompt-if-ready
    (application-input-controller)
    boolean)
(defun application-input-controller--open-prompt-if-ready (controller)
  "Open CONTROLLER's prompt block whenever it still accepts terminal input."
  (let ((ready-p
          (with-lock-held ((application-input-controller-lock controller))
            (not (application-input-controller-stopping-p controller)))))
    (and ready-p
         (terminal-ui-open-prompt-block
          (application-ui
           (application-input-controller-application controller))))))

(-> application-input-controller--call-with-responsive-prompt-block
    (application-input-controller function)
    t)
(defun application-input-controller--call-with-responsive-prompt-block
    (controller function)
  "Call FUNCTION in one prompt block unless another interaction already executes."
  (let* ((ui
           (application-ui
            (application-input-controller-application controller)))
         (marker-p nil)
         (status   0)
         (reopen-p t)
         (result   nil))
    (application-input-controller--open-prompt-if-ready controller)
    (setf marker-p (terminal-ui-start-prompt-execution ui))
    (unwind-protect
         (handler-case
             (progn
               (setf result (funcall function))
               (when (application-input-controller--prompt-result-failure-p
                      result)
                (setf status (if (eq result ':aborted) 130 1)))
               result)
           (serious-condition (condition)
             (setf status (if (typep condition 'application-turn-cancelled) 130 1)
                   reopen-p (typep condition 'application-turn-cancelled))
             (error condition)))
      (when marker-p
        (terminal-ui-finish-prompt-block ui status)
        (when reopen-p
          (application-input-controller--open-prompt-if-ready controller))))))

(-> application-input-controller--run-responsive-lisp
    (application-input-controller string &key (:interactive-p boolean))
    keyword)
(defun application-input-controller--run-responsive-lisp (controller source &key (interactive-p t))
  "Execute immediate local Lisp SOURCE with the submitting caller's interaction policy."
  (flet ((run ()
           (application-run-lisp-input
            (application-input-controller-application controller) source :interactive-p interactive-p)))
    (if interactive-p
        (application-input-controller--call-with-responsive-prompt-block controller #'run)
        (run))))

(-> application-input-controller--handle-recalled-submission
    (application-input-controller (or string user-message-input)
     &key (:prefer-steering-p boolean))
    boolean)
(defun application-input-controller--handle-recalled-submission
    (controller input &key (prefer-steering-p t))
  "Atomically route recalled INPUT and report whether recalled work handled it.

Blank input keeps the recalled work selected. Recalled Enter may steer an active
message, while recalled Tab retains its virtual FIFO position. Both decisions are
atomic across active-turn completion races. Registered nonconflicting operations
may execute immediately; other Lisp waits for the idle boundary."
  (let* ((application
           (application-input-controller-application controller))
         (message      (input-message input))
         (text         (user-message-input-text input))
         (lisp-input-p (input-lisp-p text))
         (async-p      (input-async-lisp-p text))
         (work         (input-work-from-input input))
         (busy-action  (cond (async-p
                              ':execute)
                             (lisp-input-p
                              (application-input-controller--lisp-active-turn-action controller text))))
         (handled-p    nil)
         (changed-p    nil)
         (post-action  nil))
    (when (and work
               (application-input-controller--follow-up-editing-p controller)
               (not
                (application-input-controller--submission-storage-ready-p
                 controller input)))
      (return-from application-input-controller--handle-recalled-submission t))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((index
              (application-input-controller-follow-up-edit-index controller))
            (held-work
              (application-input-controller-follow-up-edit-work controller))
            (accepted-p nil)
            (restored-p nil))
        (when (and index held-work)
          (setf handled-p t)
          (when work
            (cond
              (async-p
               (setf post-action ':execute
                     accepted-p t))
              (message
               (let ((delivery
                        (application-input-controller--admit-primary-prompt-locked
                         controller
                         message
                         :prefer-steering-p prefer-steering-p
                         :queue-index       index)))
                 (setf accepted-p (not (null delivery))
                       restored-p (eq delivery ':queued))))
              ((application-input-controller-active-p controller)
               (cond
                 ((eq busy-action ':hold)
                  (deque-push-back
                   (application-input-controller-work-items controller)
                   work)
                  (setf post-action ':hold))
                 (t
                  (setf post-action busy-action)))
               (setf accepted-p t))
              (t
               (let ((work-items
                       (application-input-controller-work-items controller)))
                 (deque-insert work-items
                               (min index (deque-count work-items))
                               work))
               (setf accepted-p t
                     restored-p t)))
            (when accepted-p
              (unless restored-p
                (application-input-controller--remove-recalled-prefix-slot-locked
                 controller index))
              (setf (application-input-controller-follow-up-edit-index controller) nil
                    (application-input-controller-follow-up-edit-work controller) nil
                    changed-p t)
              (sb-thread:condition-broadcast
               (application-input-controller-condition-variable controller)))))))
    (when changed-p
      (application-input-controller--publish-counts controller))
    (case post-action
      (:execute
       (if async-p
           (application-run-async-lisp-input application text)
           (application-input-controller--run-responsive-lisp controller text)))
      (:apply-interactive
       (application-input-controller--schedule-interactive-apply controller input))
      (:apply
       (application-input-controller--schedule-apply controller text))
      (:hold
       (application-input-controller--present-scheduled-lisp controller text)))
    handled-p))

(-> application-input-controller--vault-input-p
    ((or string user-message-input))
    boolean)
(defun application-input-controller--vault-input-p (input)
  "Return true when INPUT is a recovery-vault control in Lisp form."
  (let ((text (user-message-input-text input)))
    (and (input-lisp-p text)
         (handler-case
             (let* ((*package* (find-package '#:autolith))
                    (form      (self-read-form text :read-eval nil)))
               (or (application-operation--immediate-vault-form-p form)
                   (not (null (member form '((vault) (vault-restore) (vault-discard) (vault-store))
                                      :test #'equal)))))
           (error () nil)))))

(-> application-input-controller--submission-storage-ready-p
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--submission-storage-ready-p (controller input)
  "Return true when INPUT may be accepted without replacing preserved state."
  (or (application-input-controller-pending-persistence-enabled-p controller)
      (application-input-controller--vault-input-p input)
      (application-input-controller--prompt-storage-ready-p controller)))

(-> application-input-controller--prompt
    (application-input-controller (or string user-message-input)
     &key (:prefer-steering-p boolean))
    boolean)
(defun application-input-controller--prompt (controller input &key (prefer-steering-p t))
  "Submit terminal INPUT through the shared input controller."
  (application-world-terminal-resume (application-input-controller-application controller))
  (nth-value 0 (application-input-controller-submit-primary-prompt
                controller input :prefer-steering-p prefer-steering-p)))

(-> application-input-controller--call-submitted-input
    (application-input-controller (or string user-message-input) function) t)
(defun application-input-controller--call-submitted-input (controller input function)
  "Run FUNCTION and publish the terminal outcome of INPUT unless its work was deferred."
  (let ((submission (user-message-input-submission input))
        (outcome    ':failed))
    (unwind-protect
         (handler-bind ((application-turn-cancelled
                          (lambda (condition)
                            (declare (ignore condition))
                            (setf outcome ':cancelled))))
           (let* ((results (multiple-value-list (funcall function)))
                  (result  (first results)))
             (setf outcome (cond
                             ((eq result ':deferred)
                              nil)
                             ((eq result ':aborted)
                              ':cancelled)
                             ((application-input-controller--prompt-result-failure-p result)
                              ':failed)
                             (t
                              ':done)))
             (values-list results)))
      (when (and submission outcome)
        (application-clients-publish
         (application-input-controller-application controller)
         (list :work-finished :kind ':submission :submission submission :outcome outcome))))))

(-> application-input-controller--handle-submission
    (application-input-controller (or string user-message-input)
     &key (:steer-p boolean) (:interactive-p boolean))
    null)
(defun application-input-controller--handle-submission (controller input &key steer-p (interactive-p t))
  "Route INPUT through canonical policy and preserve its exact terminal outcome."
  (application-input-controller--call-submitted-input
   controller input
   (lambda ()
     (application-input-controller--route-submission
      controller input :steer-p steer-p :interactive-p interactive-p)))
  nil)

(-> application-input-controller--route-submission
    (application-input-controller (or string user-message-input)
     &key (:steer-p boolean) (:interactive-p boolean))
    t)
(defun application-input-controller--route-submission (controller input &key steer-p (interactive-p t))
  "Apply canonical input policy, returning :DEFERRED only after accepting later work."
  (let* ((application (application-input-controller-application controller))
         (message     (input-message input))
         (text        (user-message-input-text input)))
    (flet ((deferred (accepted-p)
             (if accepted-p ':deferred ':failed)))
      (cond
        ((input-async-lisp-p text)
         (if (application-input-controller--submission-storage-ready-p controller input)
             (progn
               (application-world-terminal-resume application)
               (application-run-async-lisp-input application text))
             ':failed))
        (message
         (deferred (application-input-controller--prompt controller message :prefer-steering-p steer-p)))
        ((not (application-input-controller--submission-storage-ready-p controller input))
         ':failed)
        ((input-lisp-p text)
         (application-world-terminal-resume application)
         (if (application-input-controller-busy-p controller)
             (ecase (application-input-controller--lisp-active-turn-action controller text)
              (:execute
               (application-input-controller--run-responsive-lisp
                controller text :interactive-p interactive-p))
              (:apply
               (deferred (application-input-controller--schedule-apply controller input)))
              (:apply-interactive
               (deferred
                (if interactive-p
                    (application-input-controller--schedule-interactive-apply controller input)
                    (application-input-controller--schedule-lisp controller input :interactive-p nil))))
              (:hold
               (deferred (application-input-controller--schedule-lisp
                          controller input :interactive-p interactive-p))))
             (deferred (application-input-controller--enqueue
                        controller (if interactive-p ':lisp ':native-lisp) input))))
        (t
         ':done)))))

(-> application-input-controller--handle-queue-submission
    (application-input-controller (or string user-message-input))
    null)
(defun application-input-controller--handle-queue-submission (controller input)
  "Queue terminal INPUT as post-turn work through canonical prompt when prose."
  (let* ((application (application-input-controller-application controller))
         (text        (user-message-input-text input))
         (message     (input-message input)))
    (cond
      ((input-async-lisp-p text)
       (when (application-input-controller--submission-storage-ready-p controller input)
         (application-world-terminal-resume application)
         (application-run-async-lisp-input application text)))
      (message
       (application-input-controller--prompt controller message :prefer-steering-p nil))
      ((application-input-controller--submission-storage-ready-p controller input)
       (application-input-controller--queue-input controller input))))
  nil)

(-> application-input-controller--recall-follow-up
    (application-input-controller)
    boolean)
(defun application-input-controller--recall-follow-up (controller)
  "Recall CONTROLLER's newest follow-up into the editor for revision."
  (let ((work            nil)
        (steering-inputs nil)
        (queued-inputs   nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (null
                  (application-input-controller-follow-up-edit-index controller)))
        (let* ((work-items
                 (application-input-controller-work-items controller))
               (index
                 (deque-position-if
                  #'input-work-editable-p
                  work-items
                  :from-end t)))
          (when index
            (setf work (deque-remove-at work-items index)
                  (application-input-controller-follow-up-edit-index controller)
                  index
                  (application-input-controller-follow-up-edit-work controller)
                  work
                  steering-inputs
                  (deque->list
                   (application-input-controller-steering-items controller))
                  queued-inputs
                  (loop for queued-work across (deque->vector work-items)
                        for input = (second queued-work)
                        when (typep input '(or string user-message-input))
                          collect (user-message-input-text input)))))))
    (when work
      (terminal-ui-recall-follow-up
       (application-ui (application-input-controller-application controller))
       (second work)
       :steering-inputs steering-inputs
       :queued-inputs   queued-inputs))
    (not (null work))))

(-> application-input-controller--cycle-follow-up
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--cycle-follow-up (controller input)
  "Move CONTROLLER's recalled draft to the previous queued follow-up, wrapping."
  (let ((current-work    (input-work-from-input input))
        (selected-work   nil)
        (steering-inputs nil)
        (queued-inputs   nil))
    (when current-work
      (with-lock-held ((application-input-controller-lock controller))
        (let ((index
                (application-input-controller-follow-up-edit-index controller)))
          (when (and index
                     (application-input-controller-follow-up-edit-work controller))
            (let* ((work-items
                     (application-input-controller-work-items controller))
                   (current-position (min index (deque-count work-items)))
                   (selected-index
                     (or
                      (deque-position-if
                       #'input-work-editable-p
                       work-items
                       :end      current-position
                       :from-end t)
                      (deque-position-if
                       #'input-work-editable-p
                       work-items
                       :start    current-position
                       :from-end t))))
              (if selected-index
                  (let ((selected-position
                          (if (< selected-index current-position)
                              selected-index
                              (1+ selected-index))))
                    (setf selected-work
                          (deque-remove-at work-items selected-index))
                    (deque-insert
                     work-items
                     (if (< selected-index current-position)
                         (1- current-position)
                         current-position)
                     current-work)
                    (setf
                     (application-input-controller-follow-up-edit-index controller)
                     selected-position
                     (application-input-controller-follow-up-edit-work controller)
                     selected-work
                     steering-inputs
                     (deque->list
                      (application-input-controller-steering-items controller))
                     queued-inputs
                     (loop for queued-work across (deque->vector work-items)
                           for queued-input = (second queued-work)
                           when (typep queued-input
                                       '(or string user-message-input))
                             collect (user-message-input-text queued-input))))
                  (setf (application-input-controller-follow-up-edit-work controller)
                        current-work))
              (sb-thread:condition-broadcast
               (application-input-controller-condition-variable controller))))))
     (when selected-work
       (terminal-ui-recall-follow-up
        (application-ui (application-input-controller-application controller))
        (second selected-work)
        :steering-inputs steering-inputs
        :queued-inputs   queued-inputs))
     (when current-work
       (application-input-controller--publish-counts controller))
     (not (null selected-work)))))

(defmethod input-port-turn-active-p ((controller application-input-controller))
  "Return true while CONTROLLER's main thread is processing one work item."
  (application-input-controller-turn-active-p controller))

(defmethod input-port-follow-up-editing-p ((controller application-input-controller))
  "Return true while CONTROLLER has recalled one queued follow-up into the draft."
  (application-input-controller--follow-up-editing-p controller))

(defmethod input-port-apply
    ((controller application-input-controller) action &optional payload)
  "Act on input ACTION in CONTROLLER's session, whether its own reader or a client asked."
  (case action
    (:submit
     (unless (application-input-controller--handle-recalled-submission controller payload)
       (application-input-controller--handle-submission
        controller payload
        :steer-p (application-input-controller-turn-active-p controller)))
     t)
    (:queue
     (unless (application-input-controller--handle-recalled-submission
              controller payload :prefer-steering-p nil)
       (application-input-controller--handle-queue-submission controller payload))
     t)
    (:command
     (application-input-controller--handle-submission controller payload :steer-p nil)
     t)
    (:detach
     (application-input-controller--handle-submission controller "(exit)" :steer-p nil)
     t)
    (:escape
     (not (null (or (application-input-controller--turn-cancellation-active-p controller)
                    (application-input-controller--request-active-turn-cancellation
                     controller)))))
    (:cleared
     (application-input-controller--clear-follow-up-edit controller)
     t)
    (:edit-queue
     (application-input-controller--recall-follow-up controller)
     t)
    (:cycle-queue
     (application-input-controller--cycle-follow-up controller payload)
     t)
    (:end-of-input
     (application-input-controller--request-exit controller ':end-of-input)
     t)))

(-> application-input-controller--process-event
    (application-input-controller t)
    null)
(defun application-input-controller--process-event (controller event)
  "Apply terminal EVENT and publish any resulting work or exit request."
  (terminal-ui-dispatch-event
   (application-ui (application-input-controller-application controller)) controller event))

(-> application-input-controller--input-ready-p
    (application-input-controller)
    boolean)
(defun application-input-controller--input-ready-p (controller)
  "Apply pending resizes and report whether CONTROLLER's terminal has input."
  (let ((ui (application-ui (application-input-controller-application controller))))
    (when (application-input-controller-exit-requested-p controller)
      (return-from application-input-controller--input-ready-p nil))
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)
    (terminal-ui-refresh-status ui)
    (when (typep ui 'fullscreen-terminal-ui)
      (terminal-ui-fullscreen--autoscroll-tick ui))
    (terminal-input-ready-p (terminal-ui-terminal ui))))

(-> application-input-controller--reader-step
    (application-input-controller)
    (option keyword))
(defun application-input-controller--reader-step (controller)
  "Read and handle one terminal event, ending the reader during shutdown."
  (if (application-input-controller-exit-requested-p controller)
      ':stop
      (progn
        (application-input-controller--process-event
         controller (application-read-terminal-event
                     (application-ui (application-input-controller-application controller))))
        nil)))

(-> application-input-controller-reader-live-p (application-input-controller) boolean)
(defun application-input-controller-reader-live-p (controller)
  "Return true while CONTROLLER's reader thread is running."
  (input-pump-live-p (application-input-controller-input-pump controller)))

(-> application-input-controller-reader-paused-p (application-input-controller) boolean)
(defun application-input-controller-reader-paused-p (controller)
  "Return true while CONTROLLER's reader is held stopped for main-thread input."
  (input-pump-paused-p (application-input-controller-input-pump controller)))

(-> application-input-controller-exit-requested-p (application-input-controller) boolean)
(defun application-input-controller-exit-requested-p (controller)
  "Return true once CONTROLLER has been asked to stop."
  (with-lock-held ((application-input-controller-lock controller))
    (not (null (application-input-controller-stopping-p controller)))))

(-> application-input-controller--start-reader
    (application-input-controller)
    null)
(defun application-input-controller--start-reader (controller)
  "Start CONTROLLER's reader unless it is paused, stopping, or already live."
  (input-pump-start (application-input-controller-input-pump controller))
  nil)

(-> application-input-controller-reader-thread-p
    (application-input-controller)
    boolean)
(defun application-input-controller-reader-thread-p (controller)
  "Return true when the current thread is CONTROLLER's terminal reader."
  (input-pump-reader-thread-p (application-input-controller-input-pump controller)))

(-> application-input-controller-call-with-exclusive-input
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-exclusive-input
    (controller function)
  "Call modal FUNCTION as the only reader of CONTROLLER's terminal.

Work already running on the reader thread proceeds in place. Other threads
pause a live reader for the call's dynamic extent. Without a live reader, call
FUNCTION directly instead of starting one on exit. This is the owner installed
as the terminal UI's exclusive-input function."
  (let ((hub (application-client-hub (application-input-controller-application controller))))
    (if (application-input-controller-reader-live-p controller)
        (unwind-protect
             (input-pump-call-with-exclusive-input
              (application-input-controller-input-pump controller)
              (lambda ()
                (client-hub-note-change hub)
                (funcall function)))
          (client-hub-note-change hub))
        (funcall function))))

(-> application-input-controller-call-with-reader-paused
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-reader-paused
    (controller function)
  "Call FUNCTION while CONTROLLER has no competing terminal reader."
  (when (application-input-controller-reader-thread-p controller)
    (error 'configuration-error
           :message
           "Terminal-owning work cannot pause the current input reader. Submit it without EVAL-NOW so it can run after the active turn."))
  (let ((hub (application-client-hub (application-input-controller-application controller))))
    (unwind-protect
         (input-pump-call-with-input-paused
          (application-input-controller-input-pump controller)
          (lambda ()
            (client-hub-note-change hub)
            (funcall function)))
      (client-hub-note-change hub))))

(-> application--command-authorization-items
    (string pathname &key (:sandbox-available-p boolean) (:external-tool-p boolean))
    list)
(defun application--command-authorization-items
    (command directory &key sandbox-available-p external-tool-p)
  "Return the shared permission choices, excluding sandbox for external calls."
  (declare (ignore command))
  (let ((sandbox-available-p (and sandbox-available-p (not external-tool-p))))
    (append
     (list
      (list :name "auto" :value ':auto :argument nil
            :description
            (if sandbox-available-p
                "remember automatic classification for this channel: sandbox, full access, or deny"
                "remember automatic classification for this channel: full access or deny"))
      (list :name "once" :value ':once :argument nil
            :description
            (if external-tool-p
                "allow this exact external call once; no local sandbox applies"
                "allow this command once with full user privileges"))
      (list :name "always" :value ':always :argument nil
            :description
            (format nil "always allow this exact ~A in ~A"
                    (if external-tool-p "external call and arguments" "command with full user privileges")
                    (application--abbreviated-directory (namestring directory)))))
     (when sandbox-available-p
       (list (list :name "sandbox" :value ':sandbox :argument nil
                    :description "remember sandbox access for this channel")))
     (list
      (list :name "full" :value ':full :argument nil
             :description "remember full user privileges for this channel across restarts")
      (list :name "deny" :value ':deny :argument nil
            :description (if external-tool-p "do not call the external tool"
                             "do not run the command"))))))

(-> application--present-permission-decision
    (application list &key (:command string) (:directory pathname) (:denied-p boolean)) boolean)
(defun application--present-permission-decision (application entry &key command directory denied-p)
  "Attach permission ENTRY to its exact tool call, or identify the operation inline."
  (let* ((context      *tool-authorization-context*)
         (conversation (and context (tool-context-conversation context)))
         (call-key     (when (and conversation
                                  (non-empty-string-p (tool-context-call-id context)))
                         (list ':call (conversation-identifier conversation)
                               (tool-context-call-id context))))
         (ui           (application-ui application))
         (metadata     (when (and call-key ui (terminal-ui-fullscreen-p ui))
                         (list :kind ':annotation :call-key call-key :annotations entry
                               :permission-denial-p denied-p)))
         (fallback     (append entry
                               (list (terminal-span
                                      ':annotation
                                      (format nil " · ~A in ~A"
                                              (sanitize-text command :single-line-p t)
                                              (sanitize-text (namestring directory) :single-line-p t)))))))
    (if metadata
        (application-present application fallback :metadata metadata)
        (application-present application fallback))))

(-> application--apply-classified-command-permission
    (application string pathname &key (:decision keyword) (:reason string))
    keyword)
(defun application--apply-classified-command-permission
    (application command directory &key decision reason)
  "Apply DECISION for the exact operation, revalidating any sandbox grant."
  (when (and (eq decision ':sandboxed) (not (application--command-sandbox-available-p)))
    (setf decision ':deny
          reason "the workspace sandbox became unavailable"))
  (application--present-permission-decision
   application
   (list (terminal-span
          ':annotation
          (format nil "  auto-permission ~A"
                  (ecase decision
                    (:sandboxed
                     "sandbox")
                    (:full-access
                     "full access")
                    (:deny
                     (format nil "deny: ~A" (sanitize-text reason :single-line-p t)))))))
   :command command :directory directory :denied-p (eq decision ':deny))
  decision)

(-> application--current-user-instructions (application) (option string))
(defun application--current-user-instructions (application)
  "Return latest instructions with a short, complete admitted assignment when available."
  (when (slot-boundp application 'conversation)
    (let* ((item         (find-if (lambda (item)
                                   (and (json-object-p item)
                                        (eq (item-role item) ':user)))
                                 (conversation-input-items (application-conversation application))
                                 :from-end t))
           (instructions (and item (context--message-text item)))
           (context      *tool-authorization-context*)
           (agent        (and context (tool-context-agent context)))
           (job          (and (non-empty-string-p instructions)
                              (typep agent 'task-child-agent)
                              (eq (tool-context-conversation context) (agent-conversation agent))
                              (eq (tool-context-configuration context) (agent-configuration agent))
                              (slot-boundp agent 'job)
                              (task-child-agent-job agent)))
           (assignment   (and job
                              (eq (task-job-participant-state job) (agent-participant-state agent))
                              (slot-boundp job 'item)
                              (getf (task-job-item job) :task))))
      (if (and (non-empty-string-p assignment) (<= (length assignment) 2048))
          (format nil "~A~%~%Current admitted assignment (context only; current user limits take precedence):~%~A"
                  instructions assignment)
          instructions))))

(defparameter *application-command-classification-limit* 256
  "Maximum cached permission decisions under one set of user instructions.")

(-> application--command-classification-cache (application (option string)) hash-table)
(defun application--command-classification-cache (application instructions)
  "Return APPLICATION's permission cache for INSTRUCTIONS, emptied when they changed.

Decisions depend on the latest user instructions, so a new instruction makes
every earlier decision unreachable; dropping them keeps the cache bounded by
the commands of one instruction rather than the whole session."
  (let ((cache (application-command-classifications application)))
    (unless (equal instructions (application-command-classification-instructions application))
      (clrhash cache)
      (setf (application-command-classification-instructions application)
            (and instructions (copy-seq instructions))))
    (when (>= (hash-table-count cache) *application-command-classification-limit*)
      (clrhash cache))
    cache))

(-> application--automatic-command-decision (t) keyword)
(defun application--automatic-command-decision (decision)
  "Return DECISION when automatic authorization may use it, otherwise :DENY."
  (if (member decision '(:sandboxed :full-access :deny) :test #'eq)
      decision
      ':deny))

(-> application--model-command-permission
    (application string pathname &key (:sandbox-available-p boolean))
    (values keyword string))
(defun application--model-command-permission
    (application command directory
     &key (sandbox-available-p (application--command-sandbox-available-p)))
  "Return the cached or freshly inferred automatic permission for COMMAND."
  (unless (and (slot-boundp application 'provider)
               (application-provider application))
    (return-from application--model-command-permission
      (values ':deny "no provider is available to classify operations")))
  (let* ((user-instructions
           (application--current-user-instructions application))
         (cache  (application--command-classification-cache application user-instructions))
         (key
           (format nil "sandbox=~:[unavailable~;available~]~%~A~%~A"
                   sandbox-available-p
                   command
                   (namestring directory)))
         (cached (gethash key cache)))
    (if cached
        (let ((decision
                (application--automatic-command-decision (first cached))))
          (values (if (and (eq decision ':sandboxed) (not sandbox-available-p))
                      ':deny
                      decision)
                  (if (and (eq decision (first cached))
                           (non-empty-string-p (rest cached)))
                      (rest cached)
                      "the cached classifier decision was unusable")))
        (multiple-value-bind (inferred-decision inferred-reason)
            (terminal-ui-call-with-judgment
             (and (slot-boundp application 'ui) (application-ui application))
             command
             (lambda ()
               (permissions-model-classify-command
                command directory
                :provider            (application-provider application)
                :configuration       (application-configuration application)
                :sandbox-available-p sandbox-available-p
                :user-instructions   user-instructions)))
          (let* ((decision
                   (application--automatic-command-decision inferred-decision))
                 (decision
                   (if (and (eq decision ':sandboxed) (not sandbox-available-p))
                       ':deny
                       decision))
                 (reason
                   (if (and (eq inferred-decision decision)
                            (non-empty-string-p inferred-reason))
                       inferred-reason
                       "the model returned an unavailable automatic grant")))
            (setf (gethash key cache) (cons decision reason))
            (values decision reason))))))

(-> application--auto-command-permission
    (application string pathname)
    keyword)
(defun application--auto-command-permission (application command directory)
  "Classify COMMAND without ever deferring automatic authorization to a picker."
  (multiple-value-bind (decision reason)
      (application--model-command-permission application command directory)
    (application--apply-classified-command-permission
     application command directory :decision decision
                                   :reason   reason)))

(-> application--apply-command-authorization-choice
    (application string pathname
     &key (:choice (option keyword)) (:kind keyword) (:request (option string)))
    keyword)
(defun application--apply-command-authorization-choice
    (application command directory &key choice (kind ':command) request)
  "Apply a shared permission CHOICE, rejecting unavailable grants."
  (case choice
    (:auto
     (application-set-channel-permission-mode application ':auto)
     (multiple-value-bind (decision reason)
         (if (eq kind ':external-tool)
             (application--model-command-permission
              application request directory :sandbox-available-p nil)
             (application--model-command-permission application command directory))
       (application--apply-classified-command-permission
        application (or request command) directory :decision decision
                                                   :reason   reason)))
    (:full
     (application-set-channel-permission-mode application ':full-access)
     ':full-access)
    (:once
     ':full-access)
    (:always
     (permissions-allow
      :configuration (application-configuration application)
      :state         (application-permission-state application)
      :command       command
      :directory     directory
      :kind          kind)
     ':full-access)
    (:sandbox
     (if (and (eq kind ':command) (application--command-sandbox-available-p))
         (progn
           (application-set-channel-permission-mode application ':sandboxed)
           ':sandboxed)
         ':deny))
    (otherwise
     ':deny)))

(-> application--ask-command-permission
    (application string pathname)
    keyword)
(defun application--ask-command-permission (application command directory)
  "Ask interactively how COMMAND may run in DIRECTORY or signal an explicit failure."
  (block nil
    (let ((controller (application-input-controller application)))
      (unless (application-approval-possible-p application)
        (error 'command-authorization-unavailable
               :message
               "Command approval is required, but no terminal or client can answer it. Attach a controlling terminal or a client, or choose an explicit permission mode; the command was not run."
               :command   command
               :directory directory))
      ;; Authorization always pauses the reader explicitly: immediate work on
      ;; the reader thread is refused a modal instead of blocking input there.
      (let* ((sandbox-available-p
               (application--command-sandbox-available-p))
             (choice
               (application-input-controller-call-with-reader-paused
                controller
                (lambda ()
                  (application-select-approval
                   application
                   :kind  ':command
                   :title (format nil "run ~A"
                                  (text-cell-prefix
                                   (sanitize-text command :single-line-p t)
                                   56))
                   :entry (format nil "Run ~A in ~A" command (namestring directory))
                   :items (application--command-authorization-items
                           command directory
                           :sandbox-available-p sandbox-available-p))))))
        (application--apply-command-authorization-choice
         application command directory :choice choice)))))

(-> application-authorize-command (application string pathname) keyword)
(-> application--authorize-operation
    (application string pathname
     &key (:kind keyword) (:request (option string)) (:ask function))
    keyword)
(defun application--authorize-operation
    (application key directory &key (kind ':command) request ask)
  "Resolve the shared session, saved-rule, automatic, and interactive policy.

KEY identifies the exact operation within KIND. REQUEST is its classifier view.
External tools cannot receive a local command sandbox grant. ASK owns the UI."
  (let ((external-p (eq kind ':external-tool)))
    (labels ((saved-p ()
               (permissions-allowed-p (application-permission-state application)
                                      key directory :kind kind))

             (classify ()
               (multiple-value-bind (decision reason)
                   (if external-p
                       (application--model-command-permission
                        application request directory :sandbox-available-p nil)
                       (application--model-command-permission application key directory))
                 (application--apply-classified-command-permission
                  application (or request key) directory :decision decision
                                                         :reason   reason))))
      (case (application-permission-mode application)
        (:full-access
         ':full-access)
        (:sandboxed
         (if external-p
             (if (saved-p) ':full-access (funcall ask))
             (if (application--command-sandbox-available-p)
                 ':sandboxed
                 (funcall ask))))
        (:auto
         (if (saved-p) ':full-access (classify)))
        (:ask
         (if (saved-p)
             ':full-access
             (if (application-approval-possible-p application)
                 (funcall ask)
                 (classify))))
        (otherwise
         ':deny)))))

(defun application-authorize-command (application command directory)
  "Return the session, saved, classified, or interactively selected permission."
  (with-lock-held ((application-command-authorization-lock application))
    (application--authorize-operation
     application command directory
     :ask (lambda ()
            (application--ask-command-permission application command directory)))))

(-> application--tool-authorization-title (tool) string)
(defun application--tool-authorization-title (tool)
  "Return the readable external identity of TOOL for its approval picker."
  (format nil "allow ~{~A~^.~}"
          (mapcar (lambda (field) (sanitize-text (second field) :single-line-p t))
                  (tool-authorization-identity-fields tool))))

(-> application--tool-authorization-request
    (tool json-object) string)
(defun application--tool-authorization-request (tool arguments)
  "Describe an unsandboxed external call as untrusted classifier input."
  (format nil "External tool call, not a shell command. No local sandbox applies.~%Identity: ~S~%Description: ~A~%Arguments: ~A"
          (tool-authorization-identity-fields tool)
          (tool-description tool)
          (json-encode arguments)))

(-> application--tool-permission-key (tool json-object) string)
(defun application--tool-permission-key (tool arguments)
  "Hash exact identity and typed arguments without persisting argument secrets."
  (labels ((canonical (value)
             (cond
               ((json-object-p value)
                (list ':object
                      (sort (loop for key being the hash-keys of value
                                  using (hash-value child)
                                  collect (cons key (canonical child)))
                            #'string< :key #'first)))
               ((stringp value)
                value)
               ((vectorp value)
                (list ':array (map 'list #'canonical value)))
               (t
                value))))
    (memory-source-text-digest
     (with-standard-io-syntax
       (let ((*print-pretty* nil))
         (prin1-to-string
          (list (tool-authorization-identity-fields tool)
                (canonical arguments)))))
     :babel-utf8-p t)))

(-> application--pretty-tool-arguments (t) string)
(defun application--pretty-tool-arguments (arguments)
  "Return ARGUMENTS as structurally pretty JSON."
  (json-encode-pretty arguments :indentation 2))

(-> application--tool-authorization-request-entry (tool json-object) list)
(defun application--tool-authorization-request-entry (tool arguments)
  "Return colored identity and complete arguments aligned by terminal cells."
  (let* ((fields
           (loop for (label value) in
                 (append
                  (tool-authorization-identity-fields tool)
                  (loop for key in (sort (loop for key being the hash-keys of arguments
                                              collect key)
                                        #'string<)
                        collect (list key (application--pretty-tool-arguments
                                           (json-get arguments key)))))
                 collect (list (sanitize-text label :single-line-p t) value)))
         (width (min 24 (loop for (label nil) in fields
                             maximize (text-cell-width label)))))
    (append
     (list (terminal-span ':tool "◇ external tool permission")
           (terminal-span ':plain (string #\Newline)))
     (loop for (label value) in fields
           for separate-label-p = (> (text-cell-width label) width)
           append
           (append
            (when separate-label-p
              (list (terminal-span ':dim (format nil "  ~A~%" label))))
            (loop for line in (uiop:split-string value :separator '(#\Newline))
                  for first-p = t then nil
                  append
                  (list (terminal-span
                         ':dim
                         (concatenate 'string "  "
                                      (termdown:fit-text
                                       (if (and first-p (not separate-label-p)) label "")
                                       width)
                                      "  "))
                        (terminal-span ':code (sanitize-text line :single-line-p t))
                        (terminal-span ':plain (string #\Newline))))))
     (when (zerop (hash-table-count arguments))
       (list (terminal-span ':dim (format nil "  no arguments~%"))))
     (list (terminal-span ':dim "  external execution is not confined by the local command sandbox")))))

(-> application--ask-tool-permission
    (application tool json-object)
    keyword)
(defun application--ask-tool-permission (application tool arguments)
  "Use the normal permission picker for an unsandboxed external call."
  (unless (application-approval-possible-p application)
    (return-from application--ask-tool-permission ':deny))
  (let* ((controller (application-input-controller application))
         (directory  (config :working-directory (application-configuration application)))
         (key        (application--tool-permission-key tool arguments))
         (choice
           (application-input-controller-call-with-reader-paused
            controller
            (lambda ()
              (application-present
               application
               (application--tool-authorization-request-entry tool arguments))
              (application-select-approval
               application
               :kind  ':external-tool
               :title (application--tool-authorization-title tool)
               :entry (application--tool-authorization-request tool arguments)
               :items (application--command-authorization-items
                       key directory :external-tool-p t))))))
    (application--apply-command-authorization-choice
     application key directory
     :choice  choice
     :kind    ':external-tool
     :request (application--tool-authorization-request tool arguments))))

(-> application-authorize-tool (application tool json-object) keyword)
(defun application-authorize-tool (application tool arguments)
  "Apply shared session and saved permissions to one exact external tool call."
  (with-lock-held ((application-command-authorization-lock application))
    (if (eq (application--authorize-operation
             application
             (application--tool-permission-key tool arguments)
             (config :working-directory (application-configuration application))
             :kind    ':external-tool
             :request (application--tool-authorization-request tool arguments)
             :ask     (lambda ()
                        (application--ask-tool-permission application tool arguments)))
            ':full-access)
        ':allow
        ':deny)))

(-> application-input-controller-create
    (application
     &key (:initial-work-items list)
          (:project-adaptation-offer-p boolean)
          (:load-pending-p boolean)
          (:pending-persistence-enabled-p boolean)
          (:start-reader-p boolean))
    application-input-controller)
(defun application-input-controller-create
    (application
     &key initial-work-items project-adaptation-offer-p (load-pending-p t)
          (pending-persistence-enabled-p t) (start-reader-p t))
  "Create CONTROLLER for APPLICATION and optionally start its terminal reader."
  (let* ((controller
           (make-instance 'application-input-controller
                          :application                application
                          :initial-work-items
                          (make-deque
                           :initial-contents (copy-tree initial-work-items))
                          :project-adaptation-offer-p project-adaptation-offer-p
                          :pending-persistence-enabled-p
                          pending-persistence-enabled-p
                          :main-thread                (current-thread))))
    (setf (application-input-controller application) controller)
    (when (slot-boundp application 'ui)
      (setf (terminal-ui-exclusive-input-function (application-ui application))
            (lambda (function)
              (application-input-controller-call-with-exclusive-input
               controller function))))
    (when load-pending-p
      (application-input-controller--load-pending controller))
    (application-job-completions-connect application)
    (application-input-controller--publish-counts controller)
    (when start-reader-p
      (application-input-controller--open-prompt-if-ready controller)
      (application-input-controller--start-reader controller))
    controller))

(-> application-input-controller--take-queued-work-locked
    (application-input-controller)
    (option list))
(defun application-input-controller--take-queued-work-locked (controller)
  "Take the next ordinary FIFO work item while CONTROLLER's lock is held."
  (let ((application (application-input-controller-application controller)))
    (when (and (not (application-input-controller-active-p controller))
               (not (application-input-controller-stopping-p controller))
               (not (application-input-controller-queued-work-paused-p controller))
               (not (application-world-terminal-paused-p application))
               (not (application-world-held-p application))
               (not (eql (application-input-controller-follow-up-edit-index controller)
                         0))
               (not (deque-empty-p
                     (application-input-controller-work-items controller))))
      (let ((work nil))
        (sb-sys:without-interrupts
          (setf work (deque-pop-front (application-input-controller-work-items controller))
                (application-input-controller-active-p controller) t
                (application-input-controller-active-work-kind controller) (first work)
                (application-input-controller-active-promoted-steering-p controller)
                (and (eq (first work) ':message)
                     (plusp (application-input-controller-steering-promotion-prefix-count controller)))
                (application-input-controller-active-work-interactive-p controller) t)
          (if (eq (first work) ':message)
              (setf (application-input-controller-active-work controller) work
                    (application-input-controller-active-work-identifier controller) (make-identifier))
              (setf (application-input-controller-active-work controller) nil
                    (application-input-controller-active-work-identifier controller) nil))
          (when (application-input-controller-follow-up-edit-index controller)
            (decf (application-input-controller-follow-up-edit-index controller)))
          (when (plusp (application-input-controller-steering-promotion-prefix-count controller))
            (decf (application-input-controller-steering-promotion-prefix-count controller))))
        work))))

(-> application-input-controller-pending-work-p
    (application-input-controller)
    boolean)
(defun application-input-controller-pending-work-p (controller)
  "Return whether ordinary FIFO work is ready for nonblocking dispatch."
  (with-lock-held ((application-input-controller-lock controller))
    (and (not (application-input-controller-active-p controller))
         (not (application-input-controller-stopping-p controller))
         (not (application-input-controller-queued-work-paused-p controller))
         (not (application-world-terminal-paused-p
               (application-input-controller-application controller)))
         (not (application-world-held-p
               (application-input-controller-application controller)))
         (not (eql (application-input-controller-follow-up-edit-index controller)
                   0))
         (not (deque-empty-p
               (application-input-controller-work-items controller))))))

(-> application-input-controller-goal-input-pending-p
    (application-input-controller) boolean)
(defun application-input-controller-goal-input-pending-p (controller)
  "Return whether input can be admitted after the current goal turn finishes."
  (with-lock-held ((application-input-controller-lock controller))
    (and (not (application-input-controller-stopping-p controller))
         (not (application-input-controller-queued-work-paused-p controller))
         (not (application-world-terminal-paused-p
               (application-input-controller-application controller)))
         (not (eql (application-input-controller-follow-up-edit-index controller) 0))
         (or (not (deque-empty-p
                   (application-input-controller-work-items controller)))
             (not (deque-empty-p
                   (application-input-controller-steering-items controller)))
             (not (deque-empty-p
                   (application-input-controller-steering-in-flight-items controller)))))))

(-> application-input-controller-take-queued-work
    (application-input-controller)
    (option list))
(defun application-input-controller-take-queued-work (controller)
  "Nonblockingly take ordinary FIFO work and publish durable state first."
  (let ((work       nil)
        (promoted-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf promoted-p
            (plusp (application-input-controller-steering-promotion-prefix-count controller))
            work (application-input-controller--take-queued-work-locked controller)))
    (when work
      (handler-case
          (progn
            (application-input-controller--persist-pending controller :error-p t)
            (application-input-controller--publish-counts controller))
        (error (condition)
          (with-lock-held ((application-input-controller-lock controller))
            (when (application-input-controller-active-p controller)
              (deque-push-front
               (application-input-controller-work-items controller) work)
              (when promoted-p
                (incf (application-input-controller-steering-promotion-prefix-count controller)))
              (when (application-input-controller-follow-up-edit-index controller)
                (incf (application-input-controller-follow-up-edit-index controller)))
              (setf (application-input-controller-active-p controller) nil
                    (application-input-controller-active-work-kind controller) nil
                    (application-input-controller-active-promoted-steering-p controller) nil
                    (application-input-controller-active-work controller) nil
                    (application-input-controller-active-work-identifier controller) nil
                    (application-input-controller-active-work-interactive-p controller) nil)))
          (error condition))))
    work))

(-> application-input-controller-begin-external-work
    (application-input-controller &key (:kind keyword))
    boolean)
(defun application-input-controller-begin-external-work
    (controller &key (kind ':message))
  "Mark one runner-free external turn active without consuming FIFO work.

A caller running the external work binds
*APPLICATION-TURN-CANCELLATION-DELIVERABLE-P* inside its own
APPLICATION-TURN-CANCELLED handler; otherwise an asynchronous cancellation
stays pending until FINISH-EXTERNAL-WORK."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (setf (slot-value controller 'main-thread) (current-thread)
            (application-input-controller-active-p controller) t
            (application-input-controller-active-work-kind controller) kind
            (application-input-controller-primary-steering-p controller)
            (eq kind ':message))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller))
      t)))

(-> application-input-controller-finish-external-work
    (application-input-controller &key (:pause-p boolean))
    null)
(defun application-input-controller-finish-external-work
    (controller &key (pause-p nil))
  "Finish a runner-free external turn using native promotion and cleanup."
  (unwind-protect
       (progn
         (with-lock-held ((application-input-controller-lock controller))
           (setf (application-input-controller-primary-steering-p controller) nil)
           (when pause-p
             (setf (application-input-controller-queued-work-paused-p controller) t))
           (when (application-input-controller-active-work controller)
             (deque-push-front
              (application-input-controller-work-items controller)
              (application-input-controller-active-work controller))
             (incf (application-input-controller-steering-promotion-prefix-count controller))
             (when (application-input-controller-follow-up-edit-index controller)
               (incf (application-input-controller-follow-up-edit-index controller)))))
         (application-input-controller--finish-work controller)
         (application-input-controller--persist-pending controller :error-p t))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (slot-value controller 'main-thread) nil)))
  nil)

(-> application-input-controller--take-work-or-idle-wakeup
    (application-input-controller) (option list))
(defun application-input-controller--take-work-or-idle-wakeup (controller)
  "Take the next work item, an idle wakeup marker, or NIL after exit."
  (let ((application (application-input-controller-application controller))
        (work        nil)
        (persist-p   nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-active-work-kind controller) nil
            (application-input-controller-active-work-interactive-p controller) nil)
      (loop
        (when (or (application-input-controller-failure controller)
                  (application-input-controller-stopping-p controller))
          (return))
        (cond
          ((and (not (application-world-terminal-paused-p application))
                (not
                 (deque-empty-p
                  (application-input-controller-initial-work-items controller))))
           (setf work
                 (deque-pop-front
                  (application-input-controller-initial-work-items controller))
                 (application-input-controller-active-p controller) t)
           (return))
          ((and (not (application-world-terminal-paused-p application))
                (not
                 (application-input-controller-queued-work-paused-p controller))
                (not
                 (deque-empty-p
                  (application-input-controller-pending-apply-items
                   controller))))
           ;; Boundary-applying commands missed their turn boundary; apply
           ;; them before any queued follow-up starts new work.
           (setf work (list ':apply-pending)
                 (application-input-controller-active-p controller) t)
           (return))
          ((setf work
                  (application-input-controller--take-queued-work-locked controller))
           ;; Publish active WORK after releasing the controller lock.
           (setf persist-p t)
           (return)))
        ;; A checkpoint waiting for the work under way holds the turns that
        ;; job completions and maintenance would start.
        (when (and (not (application-world-held-p application))
                   (setf work
                         (or (and (null (application-input-controller-follow-up-edit-index controller))
                                  (application-job-completions--take-work controller))
                             (application-job-completions--take-maintenance controller))))
          (setf (application-input-controller-active-p controller) t)
          (return))
        (let* ((cache-wait        (application-input-controller--idle-compaction-wait controller))
               (deadline          (application-job-completions--next-time controller))
               (completion-wait   (and deadline (max 1/100 (- deadline (get-universal-time)))))
               (cancellation-wait (and (application-job-completions--presentation-admissible-p controller)
                                       (task-cancellation-scan-wait (application-agent application))))
               (waits             (remove nil (list completion-wait cache-wait cancellation-wait)))
               (wait-seconds      (and waits (apply #'min waits))))
         (when (and cache-wait (zerop cache-wait))
           (setf work (list ':idle-compaction (application-idle-compaction-episode application))
                 (application-input-controller-active-p controller) t)
           (return))
         (when (application-input-controller-idle-wakeup-armed-p controller)
           (when (getf (world-terminal--activity-locked application controller) :idle-p)
             (setf (application-input-controller-idle-wakeup-armed-p controller) nil
                   work (list ':idle-wakeup))
             (return)))
         (if wait-seconds
             (condition-wait
              (application-input-controller-condition-variable controller)
              (application-input-controller-lock controller)
              :timeout wait-seconds)
             (condition-wait
              (application-input-controller-condition-variable controller)
              (application-input-controller-lock controller)))))
      (when (and work (not (eq (first work) ':idle-wakeup)))
        (setf (application-input-controller-idle-wakeup-armed-p controller) t
              (application-input-controller-active-work-kind controller) (first work)))
      (when (application-input-controller-failure controller)
        (error
         'application-input-failed
         :original-condition (application-input-controller-failure controller)
         :backtrace          (application-input-controller-failure-backtrace controller))))
    (when persist-p
      (application-input-controller--persist-pending controller))
    (application-input-controller--publish-counts controller)
    work))

(-> application-input-controller--next-work (application-input-controller) (option list))
(defun application-input-controller--next-work (controller)
  "Wait for CONTROLLER's next work item. Send idle wakeups outside the controller lock."
  (loop for work = (application-input-controller--take-work-or-idle-wakeup controller)
        while (eq (first work) ':idle-wakeup)
        do (application-world-notify-idle (application-input-controller-application controller))
        finally (return work)))

(-> application-input-controller--continuation-work-p ((option keyword)) boolean)
(defun application-input-controller--continuation-work-p (kind)
  "Return whether KIND can resume model work without a new terminal submission."
  (not (null (member kind '(:job-completion :operation-request)))))

(-> application-input-controller--finish-work
    (application-input-controller)
    null)
(defun application-input-controller--finish-work (controller)
  "Finish current work and promote unconsumed steering at its ordered position."
  (let ((application    (application-input-controller-application controller))
        (clear-notice-p nil)
        (pause-notice   nil)
        (marker-work-p  nil)
        (continuation-p nil)
        (marker-status  0)
        (reopen-p       nil))
    (with-lock-held ((application-input-controller-lock controller))
      (sb-sys:without-interrupts
        (unless (application-input-controller-stopping-p controller)
          (let* ((work-items
                   (application-input-controller-work-items controller))
                 (in-flight
                   (application-input-controller-steering-in-flight-items controller))
                 (steering
                   (application-input-controller-steering-items controller))
                 (steering-promotion-prefix-count
                   (min
                    (application-input-controller-steering-promotion-prefix-count controller)
                    (deque-count work-items)))
                 (promoted-count
                   (+ (deque-count in-flight) (deque-count steering)))
                 (suffix
                   (deque-split-at work-items steering-promotion-prefix-count)))
            (deque-move-all
             in-flight work-items
             :key (lambda (entry) (list ':message (agent-steering-input-content entry))))
            (deque-move-all steering work-items :key (lambda (input) (list ':message input)))
            (deque-move-all suffix work-items)
            (when (application-input-controller-follow-up-edit-index controller)
              (incf (application-input-controller-follow-up-edit-index controller) promoted-count))
            (setf (application-input-controller-steering-promotion-prefix-count controller)
                  (+ steering-promotion-prefix-count promoted-count))))
        (when (application-input-controller-turn-cancellation-p controller)
          (setf (application-input-controller-prompt-marker-status controller) 130))
        (setf clear-notice-p
              (application-input-controller-turn-cancellation-p controller)
              marker-work-p
              (application-input-controller-prompt-marker-work-p controller)
              continuation-p
              (application-input-controller--continuation-work-p
               (application-input-controller-active-work-kind controller))
              marker-status
              (application-input-controller-prompt-marker-status controller)
              reopen-p
              (application-input-controller-prompt-marker-reopen-p controller))
        (when (and (application-input-controller-completed-turn-p controller)
                   (zerop marker-status)
                   (application-input-controller-project-adaptation-offer-p controller)
                   (application--project-adaptation-offer-idle-locked-p application controller))
          (deque-push-back (application-input-controller-work-items controller)
                           (list ':project-adaptation-offer))
          (setf (application-input-controller-project-adaptation-offer-p controller) nil))
        (setf (application-input-controller-active-p controller) nil
              (application-input-controller-active-work-kind controller) nil
              (application-input-controller-active-work controller) nil
              (application-input-controller-active-work-identifier controller) nil
              (application-input-controller-active-work-interactive-p controller) nil
              (application-input-controller-prompt-marker-work-p controller) nil
              (application-input-controller-prompt-marker-status controller) 0
              (application-input-controller-completed-turn-p controller) nil
              (application-input-controller-prompt-marker-reopen-p controller) nil
              (application-input-controller-turn-cancellation-p controller) nil
              (application-input-controller-turn-cancellation-delivery-pending-p controller) nil))
      (when (and
             (application-input-controller-queued-work-paused-p controller)
             (not
              (deque-empty-p
               (application-input-controller-work-items controller))))
        (setf pause-notice
              (format nil
                      "interrupted; ~D queued item~:P held; submit input to resume"
                      (deque-count
                       (application-input-controller-work-items controller)))))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (cond (pause-notice
           (when-let ((ui (application-ui
                           (application-input-controller-application controller))))
             (terminal-ui-set-notice
              ui pause-notice
              :duration-seconds *application-interrupted-queue-notice-seconds*)))
          (clear-notice-p
           (when-let ((ui (application-ui
                           (application-input-controller-application controller))))
             (terminal-ui-set-notice ui nil))))
    (application-input-controller--publish-counts controller)
    (let ((ui
            (application-ui
             (application-input-controller-application controller))))
      (when (and ui continuation-p)
        (terminal-ui-report-program-status
         ui (terminal--program-status-outcome marker-status)))
      (when (and ui marker-work-p)
        (terminal-ui-finish-prompt-block ui marker-status))
      (when (and ui reopen-p)
        (application-input-controller--open-prompt-if-ready controller))))
  nil)

(-> application-input-controller-stop (application-input-controller) null)
(defun application-input-controller-stop (controller)
  "Retire CONTROLLER after shutdown work is complete and join its reader."
  (when (eq controller
            (application-input-controller
             (application-input-controller-application controller)))
    (application-job-completions-disconnect
     (application-input-controller-application controller)))
  (let ((cancelled nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf cancelled (application-input-controller--clear-queues-locked controller))
     (setf (application-input-controller-stopping-p controller) t
           (application-input-controller-active-p controller) nil
           (application-input-controller-active-work-kind controller) nil
           (application-input-controller-active-work controller) nil
           (application-input-controller-active-work-identifier controller) nil
           (application-input-controller-active-work-interactive-p controller) nil
           (application-input-controller-prompt-marker-work-p controller) nil
           (application-input-controller-prompt-marker-status controller) 0
           (application-input-controller-prompt-marker-reopen-p controller) nil
           (application-input-controller-pending-snapshot-identifier controller) nil
           (application-input-controller-vault-capture-identifiers controller) nil
           (application-input-controller-steering-promotion-prefix-count controller) 0
           (application-input-controller-queued-work-paused-p controller) nil
           (application-input-controller-follow-up-edit-index controller) nil
           (application-input-controller-follow-up-edit-work controller) nil
           (application-input-controller-turn-cancellation-p controller) nil
           (application-input-controller-turn-cancellation-delivery-pending-p
            controller)
          nil)
     (sb-thread:condition-broadcast
      (application-input-controller-condition-variable controller)))
    (dolist (work cancelled)
      (job-cancel work :reason ':shutdown)))
  (input-pump-stop (application-input-controller-input-pump controller))
  (let ((application (application-input-controller-application controller)))
    (when (eq controller (application-input-controller application))
      (setf (application-input-controller application) nil)
      (when (slot-boundp application 'ui)
        (setf (terminal-ui-exclusive-input-function (application-ui application))
              #'funcall))))
  nil)

(-> application--record-turn-aborted
    (application serious-condition
     &key (:turn-start-sequence (integer 1))
          (:reason (member :cancelled :agent-loop :application-error)))
    (option list))
(defun application--record-turn-aborted
    (application condition &key turn-start-sequence reason)
  "Repair and publish APPLICATION's durable partial turn after CONDITION."
  (let ((conversation (application-conversation application)))
    (conversation--repair-incomplete-tool-calls conversation)
    (conversation-append-turn-aborted
     conversation
     :turn-start-sequence turn-start-sequence
     :reason              reason
     :condition-type      (format nil "~S" (class-name (class-of condition)))
     :message             (bounded-string condition
                                          :limit
                                          *conversation-turn-aborted-message-maximum-characters*)
     :request-number
     (and (typep condition 'agent-loop-error)
          (agent-loop-error-request-number condition)))))

(-> application--call-with-model-error-handling
    (application function &key (:fatal-agent-loop-errors-p boolean)
                               (:record-turn-aborted-p boolean))
    keyword)
(defun application--call-with-model-error-handling
    (application function &key (fatal-agent-loop-errors-p t) (record-turn-aborted-p t))
  "Run model work under the expected, cancellation, and fatal condition boundary.

Maintenance callers disable RECORD-TURN-ABORTED-P because they do not own a
user model turn, while retaining cancellation and error presentation."
  (let* ((conversation        (application-conversation application))
         (turn-start-sequence (conversation-next-sequence conversation))
         (signal-backtrace    nil))
    ;; The backtrace handler must sit inside HANDLER-CASE: its handlers are
    ;; tried first and unwind before any enclosing handler could look.
    (handler-case
        (handler-bind
            ((serious-condition
               (lambda (condition)
                 (declare (ignore condition))
                 (setf signal-backtrace (application-safe-backtrace)))))
          (progn
            (funcall function)
            ':continue))
      (application-turn-cancelled (condition)
        (when record-turn-aborted-p
          (application--record-turn-aborted
           application condition
           :turn-start-sequence turn-start-sequence
           :reason              ':cancelled))
        (error condition))
      (application-input-failed (condition)
        (error condition))
      (agent-loop-error (condition)
        (when record-turn-aborted-p
          (application--record-turn-aborted
           application condition
           :turn-start-sequence turn-start-sequence
           :reason              ':agent-loop))
        (if fatal-agent-loop-errors-p
            (application-raise-fatal
             application condition signal-backtrace)
            (progn
              (application-handle-expected-error application condition)
              ':failed)))
      ((or conversation-invariant-error
           active-image-corruption)
       (condition)
       (application-raise-fatal application condition signal-backtrace))
      ((or autolith-error cl-llm-provider-api:provider-api-error) (condition)
        (when record-turn-aborted-p
          (application--record-turn-aborted
           application condition
           :turn-start-sequence turn-start-sequence
           :reason              ':application-error))
        (application-handle-expected-error application condition)
        ':failed)
      (serious-condition (condition)
        (application-raise-fatal application condition signal-backtrace)))))

(-> application--run-message-input
    (application (or string user-message-input)
     &key (:steering-function (option function))
          (:initial-steering-function (option function))
          (:steering-persisted-function (option function))
          (:user-message-persisted-function (option function))
          (:pending-operations-function (option function))
          (:pending-input-identifier (option non-empty-string))
          (:tools-p boolean)
          (:tool-allowlist (option list))
          (:tool-restriction-p boolean)
          (:goal-continuations-p boolean)
          (:fatal-agent-loop-errors-p boolean))
    keyword)
(defun application--run-message-input
    (application input
     &key steering-function steering-persisted-function
          initial-steering-function
          user-message-persisted-function pending-operations-function
          pending-input-identifier (tools-p t)
          tool-allowlist (tool-restriction-p nil) (goal-continuations-p t)
          (fatal-agent-loop-errors-p t))
  "Run model INPUT with established expected, cancellation, and fatal handling.

Sharing is requested when a primary prompt is accepted and may wait until
its user message is durable. Replaying the turn does not repeat the
submission-time sharing request."
  (application--call-with-model-error-handling
   application
   (lambda ()
     (application-run-message
      application input
      :steering-function               steering-function
      :initial-steering-function       initial-steering-function
      :steering-persisted-function     steering-persisted-function
      :user-message-persisted-function user-message-persisted-function
      :pending-operations-function     pending-operations-function
      :pending-input-identifier        pending-input-identifier
      :tools-p                         tools-p
      :tool-allowlist                  tool-allowlist
      :tool-restriction-p              tool-restriction-p
      :goal-continuations-p            goal-continuations-p))
   :fatal-agent-loop-errors-p fatal-agent-loop-errors-p))

(-> application-input-controller--interactive-prompt-work-p (list) boolean)
(defun application-input-controller--interactive-prompt-work-p (work)
  "Return true when WORK represents one foreground user interaction."
  (not (null (member (first work) '(:lisp :message) :test #'eq))))

(-> application-input-controller--begin-prompt-work
    (application-input-controller list)
    null)
(defun application-input-controller--begin-prompt-work (controller work)
  "Initialize prompt-marker state and begin execution for interactive WORK."
  (setf (application-input-controller-prompt-marker-work-p controller) nil
        (application-input-controller-prompt-marker-status controller) 0
        (application-input-controller-completed-turn-p controller) nil
        (application-input-controller-prompt-marker-reopen-p controller) t)
  (when (application-input-controller--continuation-work-p (first work))
    (let* ((application (application-input-controller-application controller))
           (ui          (and (slot-boundp application 'ui) (application-ui application))))
      (when ui
        (terminal-ui-report-program-status ui ':working))))
  (when (and
         (application-input-controller-active-work-interactive-p controller)
         (application-input-controller--interactive-prompt-work-p work))
    (application-input-controller--open-prompt-if-ready controller)
    (setf (application-input-controller-prompt-marker-work-p controller)
          (terminal-ui-start-prompt-execution
           (application-ui
            (application-input-controller-application controller)))))
  nil)

(-> application-input-controller--record-prompt-result
    (application-input-controller t)
    null)
(defun application-input-controller--record-prompt-result (controller result)
  "Record RESULT's completion status for CONTROLLER's current prompt block."
  (when (application-input-controller--prompt-result-failure-p result)
    (setf (application-input-controller-prompt-marker-status controller)
          (if (eq result ':aborted) 130 1)))
  nil)

(-> application-input-controller--record-prompt-condition
    (application-input-controller serious-condition)
    null)
(defun application-input-controller--record-prompt-condition
    (controller condition)
  "Record escaping CONDITION and whether cancellation may reopen the next prompt."
  (setf (application-input-controller-prompt-marker-status controller)
        (if (typep condition 'application-turn-cancelled) 130 1)
        (application-input-controller-prompt-marker-reopen-p controller)
        (typep condition 'application-turn-cancelled))
  nil)

(-> application-input-controller--work-submission (list) list)
(defun application-input-controller--work-submission (work)
  "Return the admitted submission identity of message, command or Lisp WORK."
  (case (first work)
    (:message
     (user-message-input-submission (second work)))
    (:lisp
     (getf (rest (rest work)) :submission))))

(-> application-input-controller--publish-work-finished (application-input-controller list) null)
(defun application-input-controller--publish-work-finished (controller work)
  "Publish WORK's exact submission outcome, retaining it for disconnected clients."
  (let ((application (application-input-controller-application controller)))
    (application-clients-publish
     application
     (list :work-finished
           :kind       (first work)
           :submission (application-input-controller--work-submission work)
           :outcome    (case (application-input-controller-prompt-marker-status controller)
                         (130
                          ':cancelled)
                         (1
                          ':failed)
                         (t
                          ':done))
           :idle-p     (with-lock-held ((application-input-controller-lock controller))
                         (and (deque-empty-p (application-input-controller-work-items controller))
                              (deque-empty-p (application-input-controller-steering-items controller))
                              t)))))
  nil)

(-> application-input-controller--run-work
    (application-input-controller list)
    null)
(defun application-input-controller--run-work (controller work)
  "Run one submitted WORK item on the application main thread.

The channel's structured clients hear when it started and when it finished,
however it ended."
  (let ((application (application-input-controller-application controller))
        (pending-input-identifier
          (with-lock-held ((application-input-controller-lock controller))
            (application-input-controller-active-work-identifier controller))))
    (unwind-protect
     (handler-case
         (progn
           (with-lock-held ((application-input-controller-lock controller))
             (unless (eq work (fourth (application-deferred-goal-continuation application)))
               (setf (application-deferred-goal-continuation application) nil)))
           (application-input-controller--begin-prompt-work controller work)
           (application-clients-publish
            application
            (append (list :work-started :kind (first work))
                    (when (application-input-controller--work-submission work)
                      (list :submission (application-input-controller--work-submission work)))))
           (application-world-before-turn application (first work))
           (case (first work)
             (:idle-compaction
              (application-input-controller--run-idle-compaction controller (second work)))
             (:apply-pending
              (application-input-controller--apply-pending-commands controller))
             (:channel-deletion
              (application--finish-channel-deletion application (second work)))
             (:client-work
              (job-run-inline (second work)))
             (:job-completion
              (application-input-controller--record-prompt-result
               controller
               (application--call-with-model-error-handling
                application
                (lambda ()
                  (application-job-completions-run controller (second work))))))
             (:job-completion-maintenance
              (application-job-completions-maintain controller (second work)))
             (:message
              (let ((result
                      (application--run-message-input
                       application
                       (second work)
                       :steering-function
                       (lambda ()
                         (application-input-controller--take-primary-steering controller))
                       :initial-steering-function
                       (lambda ()
                         (application-input-controller--take-promoted-steering controller))
                       :steering-persisted-function
                       (lambda (identifier)
                         (application-input-controller--acknowledge-primary-steering controller identifier))
                       :user-message-persisted-function
                       (lambda (identifier)
                         (application-input-controller--acknowledge-active-work
                          controller identifier)
                         ;; The first durable message gives a fresh channel
                         ;; its place in the world; a share that waited for
                         ;; it goes out now.
                         (application-world-share-pending application))
                       :pending-operations-function
                       (lambda (agent)
                         (application-input-controller--apply-pending-commands
                          controller
                          :agent agent))
                       :pending-input-identifier pending-input-identifier)))
                (application-input-controller--record-prompt-result
                 controller result)))
             ((:native-lisp :lisp)
              (let ((result
                      (if (input-async-lisp-p (second work))
                          (application-run-async-lisp-input application (second work))
                          (application-run-lisp-input
                           application (second work) :interactive-p (eq (first work) ':lisp)))))
                (application-input-controller--record-prompt-result
                 controller result)))
             (:operation-request
              (let* ((request    (second work))
                     (identifier (getf request :pending-input-identifier)))
                (if (and identifier
                         (conversation-pending-input-identifier-p
                          (application-conversation application) identifier))
                    (application-input-controller--record-prompt-result controller ':skipped)
                    (application-input-controller--record-prompt-result
                     controller
                     (application--call-with-model-error-handling
                      application
                      (lambda ()
                        (application-input-controller-call-with-primary-steering
                         controller
                         (lambda (take acknowledge)
                           (application--run-turn
                            application
                            (getf request :text)
                            :operation-request           (getf request :request)
                            :pending-input-identifier    identifier
                            :steering-function           take
                            :steering-persisted-function acknowledge
                            :pending-operations-function
                            (lambda (owner)
                              (application-input-controller--apply-pending-commands
                               controller :agent owner)))))))))))
             (:project-adaptation-offer
              (application-maybe-offer-project-adaptation application)))
           ;; Non-message input does not reset the per-message continuation budget.
           (when (and (member (first work) '(:lisp :native-lisp))
                      (application--take-deferred-goal-continuation-p application work))
             (application--call-with-model-error-handling
              application
              (lambda ()
                (application-input-controller-call-with-primary-steering
                 controller
                 (lambda (take acknowledge)
                   (application--run-goal-continuations
                    application
                    :steering-function           take
                    :steering-persisted-function acknowledge
                    :pending-operations-function
                    (lambda (agent)
                      (application-input-controller--apply-pending-commands
                       controller :agent agent)))))))))
       (serious-condition (condition)
         (application-input-controller--record-prompt-condition
          controller condition)
         (error condition)))
     (application-input-controller--publish-work-finished controller work)))
  nil)
