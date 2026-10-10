(in-package #:autolith)

;;;; -- Headless Native Input --

(-> acp-input--validate-storage (application) null)
(defun acp-input--validate-storage (application)
  "Reject unreadable pending input before publishing an empty controller snapshot."
  (let* ((configuration (application-configuration application))
         (conversation (application-conversation application))
         (path (configuration-pending-inputs-path configuration (conversation-pathname conversation)))
         (legacy (configuration-legacy-pending-inputs-path configuration))
         (current (probe-file path))
         (source (or current (probe-file legacy))))
    (when source
      (multiple-value-bind (form complete-p) (snapshot-read source)
        (let ((identifier (if current
                              (conversation-identifier conversation)
                              (and (consp form) (getf (rest form) :conversation-id)))))
          (unless (and complete-p (stringp identifier)
                       (application-input-controller--pending-state form identifier))
            (error 'agentcomms:acp-state-error
                   :message "Pending input storage needs recovery in the terminal interface."))))))
  nil)

(-> acp-input-initialize (acp-session) application-input-controller)
(defun acp-input-initialize (session)
  "Attach native pending-input storage without starting a terminal reader."
  (let* ((application (acp-session-application session))
         (controller (make-instance 'application-input-controller
                                    :application application :main-thread nil)))
    (acp-input--validate-storage application)
    (setf (application-input-controller application) controller)
    (application-input-controller--load-pending controller)
    (application-input-controller--finish-work controller)
    controller))

(-> acp-input-controller (acp-session) (option application-input-controller))
(defun acp-input-controller (session)
  "Return SESSION's native input owner, if construction has attached it."
  (application-input-controller (acp-session-application session)))

(-> acp-input--ensure-conversation (acp-session) null)
(defun acp-input--ensure-conversation (session)
  "Publish the conversation identity before acknowledging its first pending input."
  (let ((conversation (application-conversation (acp-session-application session))))
    (with-recursive-lock-held ((conversation-append-lock conversation))
      (unless (conversation-persisted-p conversation)
        (conversation-append-record conversation '(:pending-input :source :acp)))))
  nil)

(-> acp-input--submit (acp-session hash-table boolean) hash-table)
(defun acp-input--submit (session params steer-p)
  "Durably accept prose through native steering or follow-up admission."
  (let ((text (agentcomms:acp-field params "text" :type ':string :required-p t))
        (controller (acp-input-controller session)))
    (unless (non-empty-string-p (string-trim '(#\Space #\Tab #\Newline #\Return) text))
      (agentcomms:acp-invalid-params "Input text must not be blank."))
    (when (> (length text) 262144)
      (agentcomms:acp-invalid-params "Input text exceeds the character limit."))
    (unless controller
      (error 'acp-extension-unavailable :reason "The session has no input controller."))
    (sb-sys:without-interrupts
      (with-lock-held ((acp-session-lock session))
        (when (or (acp-session-closing-p session) (acp-session-closed-p session))
          (error 'acp-extension-unavailable :reason "The session is closing."))
        (when (and (acp-session-prompt-thread session) (acp-session-cancelled-p session))
          (error 'application-turn-cancelled)))
      (acp-input--ensure-conversation session)
      (multiple-value-bind (accepted-p delivery)
          (application-input-controller-submit-primary-prompt
           controller text :prefer-steering-p steer-p)
        (unless accepted-p
          (error 'agentcomms:acp-state-error
                 :message "The native input controller rejected the prompt."))
        (application-input-controller--persist-pending controller :error-p t)
        (with-lock-held ((acp-session-lock session))
          (unless (acp-session-prompt-thread session)
            (setf (acp-session-cancelled-p session) nil))
          (setf (acp-session-completion-report session) nil))
        (acp-completion-wake session)
        (agentcomms:json-object "accepted" t "delivery" (string-downcase (symbol-name delivery)))))))

(-> acp-extension-steer (acp-session hash-table) hash-table)
(defun acp-extension-steer (session params)
  "Submit primary prose for the next safe boundary, promoting late input natively."
  (acp-input--submit session params t))

(-> acp-extension-queue (acp-session hash-table) hash-table)
(defun acp-extension-queue (session params)
  "Append a primary prose prompt to the native follow-up FIFO."
  (acp-input--submit session params nil))

(-> acp-input--pending-p (acp-session) boolean)
(defun acp-input--pending-p (session)
  "Return whether SESSION has runnable native follow-up work."
  (let ((controller (acp-input-controller session)))
    (and controller (application-input-controller-pending-work-p controller))))

(-> acp-input--prepare-queued-turn (acp-session) (or null string user-message-input))
(defun acp-input--prepare-queued-turn (session)
  "Claim one pending prompt only after ACP primary admission succeeds."
  (let ((controller (acp-input-controller session)))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((work (first (deque->list (application-input-controller-work-items controller)))))
        (when (and work (not (eq (first work) ':message)))
          (error 'agentcomms:acp-state-error
                 :message "Recovered non-message input requires the terminal interface."))))
    (let ((work (application-input-controller-take-queued-work controller)))
      (when work (user-message-input-copy (second work))))))

(-> acp-input--present-user (acp-observer (or string user-message-input)) null)
(defun acp-input--present-user (observer input)
  "Project newly persisted headless input through an ordinary ACP user update."
  (with-lock-held ((acp-observer-lock observer))
    (acp-observer--send
     observer (agentcomms:acp-update-user-message
               (agentcomms:acp-text-content (user-message-input-text input)))))
  nil)

(defmethod agent-observer-take-steering ((observer acp-observer))
  "Take native steering with its durable acknowledgement identifiers."
  (let ((controller (acp-input-controller (acp-observer-session observer))))
    (when controller
      (let ((entries (application-input-controller--take-steering controller)))
        (when entries
          (application-input-controller--persist-pending controller :error-p t))
        entries))))

(defmethod agent-observer-steering-persisted ((observer acp-observer) identifier)
  "Retire and present steering only after its conversation append succeeds."
  (let ((controller (acp-input-controller (acp-observer-session observer))))
    (when controller
      (let ((entry
              (with-lock-held ((application-input-controller-lock controller))
                (find identifier
                      (deque->list (application-input-controller-steering-in-flight-items controller))
                      :key #'agent-steering-input-identifier :test #'string=))))
        (when entry
          (application-input-controller--acknowledge-steering controller identifier)
          (acp-input--present-user observer (agent-steering-input-content entry))))))
  nil)

(-> acp-input--user-persisted (acp-observer list) null)
(defun acp-input--user-persisted (observer details)
  "Acknowledge and display queued user input at its durable append boundary."
  (let* ((controller (acp-input-controller (acp-observer-session observer)))
         (identifier (getf details :pending-input-identifier))
         (work (and controller identifier
                    (with-lock-held ((application-input-controller-lock controller))
                      (application-input-controller-active-work controller)))))
    (when work
      (application-input-controller--acknowledge-active-work controller identifier)
      (acp-input--present-user observer (second work))))
  nil)

(-> acp-input-close (acp-session) null)
(defun acp-input-close (session)
  "Preserve accepted input before retiring the reader-free controller."
  (let ((controller (acp-input-controller session)))
    (when controller
      (application-input-controller--persist-pending controller :error-p t)
      (application-input-controller--prepare-shutdown controller ':disconnect)))
  nil)

(-> acp-input--pending-commands-p (acp-session) boolean)
(defun acp-input--pending-commands-p (session)
  "Return whether native boundary-applying commands await SESSION."
  (let ((controller (acp-input-controller session)))
    (and controller
         (with-lock-held ((application-input-controller-lock controller))
           (not (deque-empty-p (application-input-controller-pending-apply-items controller)))))))

(-> acp-input--apply-commands (acp-observer t) null)
(defun acp-input--apply-commands (observer agent)
  "Apply native boundary commands and project their presented text."
  (let* ((session (acp-observer-session observer))
         (controller (acp-input-controller session))
         (*application-command-presentation-capture-p* t)
         (*application-command-presentation-text* nil))
    (when controller
      (application-input-controller--apply-pending-commands controller :agent agent)
      (dolist (text (nreverse *application-command-presentation-text*))
        (with-lock-held ((acp-observer-lock observer))
          (acp-observer--send observer (agentcomms:acp-update-agent-message
                                        (agentcomms:acp-text-content text)))))
      (acp-extension-notify session "state")))
  nil)

(defmethod agent-observer-apply-pending-operations ((observer acp-observer) agent)
  "Apply headless command changes at the native provider safe boundary."
  (acp-input--apply-commands observer agent))

(-> acp-input--run-pending-commands (acp-session) null)
(defun acp-input--run-pending-commands (session)
  "Apply commands that arrived after the final provider boundary under primary admission."
  (acp-service--call-with-operation
   (acp-session-service session) session
   (lambda ()
     (let* ((application (acp-session-application session))
            (*active-application* application)
            (*configuration* (application-configuration application))
            (*default-pathname-defaults* (config :working-directory *configuration*))
            (observer (acp-observer-create session)))
       (acp-input--apply-commands observer (application-agent application))
       (acp-observer-flush observer))))
  nil)
