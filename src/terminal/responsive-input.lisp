(in-package #:autolith)

;;;; -- Responsive Terminal Input --

(defparameter *application-forced-interrupt-status* 130
  "The process status used when repeated Ctrl-C forces exit.")

(defparameter *application-interrupt-exit-window-seconds* 5/2
  "Seconds in which a second Ctrl-C may force the process to exit.")

(defparameter *application-interrupt-hint-delay-seconds* 1/4
  "Seconds a cancellation may run before its force-exit hint becomes worth showing.")

(defparameter *application-interrupted-queue-notice-seconds* 8
  "Seconds an active Ctrl-C explains that queued work remains paused.")

(-> application-input-controller--interrupt-window-text () string)
(defun application-input-controller--interrupt-window-text ()
  "Return the force-exit window as concise user-facing seconds."
  (format nil "~,1F" *application-interrupt-exit-window-seconds*))

(-> application-input-controller--forced-exit-text () string)
(defun application-input-controller--forced-exit-text ()
  "Return the repeated-Ctrl-C forced-exit explanation."
  (format nil
          "Ctrl-C pressed twice within ~A seconds; forcing Autolith to exit."
          (application-input-controller--interrupt-window-text)))

(-> application-input-controller--monotonic-seconds () real)
(defun application-input-controller--monotonic-seconds ()
  "Return monotonically increasing process time in seconds."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defparameter *application-interrupt-clock-function*
  #'application-input-controller--monotonic-seconds
  "The monotonic clock captured by newly created input controllers.")

(defvar *application-forced-exit-function*
  (lambda (status)
    (sb-ext:exit :code status :abort t))
  "The process termination boundary captured by each input controller.")

(defclass application-input-controller ()
  ((application
    :initarg :application
    :reader application-input-controller-application
    :type application
    :documentation "The application receiving terminal events and submitted work.")
   (lock
    :initform (make-lock "Autolith input controller")
    :reader application-input-controller-lock
    :type t
    :documentation "The lock protecting work, reader, and exit state.")
   (condition-variable
    :initform (make-condition-variable :name "Autolith input controller")
    :reader application-input-controller-condition-variable
    :type t
    :documentation "The main and reader thread wakeup condition.")
   (work-items
    :initform (make-deque)
    :reader application-input-controller-work-items
    :type deque
    :documentation "FIFO message and command work submitted by the reader.")
   (initial-work-items
    :initarg :initial-work-items
    :initform (make-deque)
    :reader application-input-controller-initial-work-items
    :type deque
    :documentation "Ordered startup work excluded from durable pending input.")
   (steering-items
    :initform (make-deque)
    :reader application-input-controller-steering-items
    :type deque
    :documentation "FIFO user messages waiting for the active turn's next tool boundary.")
   (steering-in-flight-items
    :initform (make-deque)
    :reader application-input-controller-steering-in-flight-items
    :type deque
    :documentation
    "Identified steering messages drained but not yet durably acknowledged.")
   (pending-apply-items
    :initform (make-deque)
    :reader application-input-controller-pending-apply-items
    :type deque
    :documentation
    "FIFO command inputs applying at the active turn's next safe boundary.")
   (follow-up-edit-index
    :initform nil
    :accessor application-input-controller-follow-up-edit-index
    :type (option (integer 0))
    :documentation
    "The recalled draft's position in the virtual queue that still includes it.")
   (follow-up-edit-work
    :initform nil
    :accessor application-input-controller-follow-up-edit-work
    :type (option list)
    :documentation "The queued work item currently recalled into the draft.")
   (later-state
    :initarg :later-state
    :reader application-input-controller-later-state
    :type later-state
    :documentation "The durable deferred inputs owned by this controller.")
   (active-p
    :initform nil
    :accessor application-input-controller-active-p
    :type boolean
    :documentation "Whether the main thread is processing one work item.")
    (active-work-kind
     :initform nil
     :accessor application-input-controller-active-work-kind
     :type (option keyword)
     :documentation
     "The kind of work currently executing, independent of durable message state.")
   (active-work
    :initform nil
    :accessor application-input-controller-active-work
    :type (option list)
    :documentation "The ordinary pending work item currently being dispatched.")
   (active-work-identifier
    :initform nil
    :accessor application-input-controller-active-work-identifier
    :type (option string)
    :documentation "The durable identifier of ACTIVE-WORK, when present.")
   (active-work-interactive-p
    :initform nil
    :accessor application-input-controller-active-work-interactive-p
    :type boolean
    :documentation "Whether ACTIVE-WORK came from the interactive prompt queue.")
   (prompt-marker-work-p
    :initform nil
    :accessor application-input-controller-prompt-marker-work-p
    :type boolean
    :documentation "Whether current work emitted an OSC 133 execution-start marker.")
   (prompt-marker-status
    :initform 0
    :accessor application-input-controller-prompt-marker-status
    :type (integer 0)
    :documentation "The completion status for current marked prompt work.")
   (prompt-marker-reopen-p
    :initform nil
    :accessor application-input-controller-prompt-marker-reopen-p
    :type boolean
    :documentation "Whether current work may open the next semantic prompt block.")
   (pending-snapshot-identifier
    :initform nil
    :accessor application-input-controller-pending-snapshot-identifier
    :type (option string)
    :documentation "The stable identifier of the current nonempty pending snapshot.")
   (vault-capture-identifiers
    :initform nil
    :accessor application-input-controller-vault-capture-identifiers
    :type list
    :documentation
    "Vault captures transactionally represented by the current pending snapshot.")
   (steering-promotion-prefix-count
    :initform 0
    :accessor application-input-controller-steering-promotion-prefix-count
    :type (integer 0)
    :documentation
    "Queued work that must remain ahead of steering submitted during active work.")
   (pending-persistence-enabled-p
    :initarg :pending-persistence-enabled-p
    :initform t
    :accessor application-input-controller-pending-persistence-enabled-p
    :type boolean
    :documentation
    "Whether this controller may replace its conversation's pending snapshot.")
   (live-vault-sync-p
    :initform nil
    :accessor application-input-controller-live-vault-sync-p
    :type boolean
    :documentation
    "Whether this session accepted new input that should create a live vault capture.")
   (queued-work-paused-p
    :initform nil
    :accessor application-input-controller-queued-work-paused-p
    :type boolean
    :documentation
    "Whether an explicit interruption is holding queued work for user review.")
   (localgroup-handoff-p
    :initform nil
    :accessor application-input-controller-localgroup-handoff-p
    :type boolean
    :documentation "Whether localgroup has stopped new input for process handoff.")
   (turn-cancellation-p
    :initform nil
    :accessor application-input-controller-turn-cancellation-p
    :type boolean
    :documentation "Whether one active turn is cancelling but not yet finished.")
   (turn-cancellation-delivery-pending-p
    :initform nil
    :accessor application-input-controller-turn-cancellation-delivery-pending-p
    :type boolean
    :documentation "Whether the main thread must still receive turn cancellation.")
   (stopping-p
    :initform nil
    :accessor application-input-controller-stopping-p
    :type boolean
    :documentation "Whether no more terminal input or work may be accepted.")
   (exit-reason
    :initform nil
    :accessor application-input-controller-exit-reason
    :type (option keyword)
    :documentation "The user-facing reason input processing stopped.")
   (reader-thread
    :initform nil
    :accessor application-input-controller-reader-thread
    :type t
    :documentation "The restartable terminal reader thread.")
   (reader-paused-p
    :initform nil
    :accessor application-input-controller-reader-paused-p
    :type boolean
    :documentation "Whether the reader must remain stopped for main-thread input.")
   (pause-depth
    :initform 0
    :accessor application-input-controller-pause-depth
    :type (integer 0)
    :documentation "Nested main-thread requests keeping the reader stopped.")
   (main-thread
    :initarg :main-thread
    :reader application-input-controller-main-thread
    :type t
    :documentation "The model and command thread interrupted for immediate exit.")
   (forced-exit-function
    :initarg :forced-exit-function
    :initform *application-forced-exit-function*
    :reader application-input-controller-forced-exit-function
    :type function
    :documentation "The process termination boundary captured before the reader starts.")
   (interrupt-clock-function
    :initarg :interrupt-clock-function
    :initform *application-interrupt-clock-function*
    :reader application-input-controller-interrupt-clock-function
    :type function
    :documentation "The monotonic clock used to recognize repeated Ctrl-C input.")
   (interrupt-deadline
    :initform nil
    :accessor application-input-controller-interrupt-deadline
    :type (option real)
    :documentation "The inclusive monotonic deadline for a force-exit Ctrl-C.")
   (interrupt-hint-time
    :initform nil
    :accessor application-input-controller-interrupt-hint-time
    :type (option real)
    :documentation "The monotonic time at which an unshown force-exit hint is due.")
   (forced-exit-message
    :initform (format nil "~%~A~%"
                      (application-input-controller--forced-exit-text))
    :accessor application-input-controller-forced-exit-message
    :type string
    :documentation "The complete plain-text notice emitted by forced shutdown.")
   (failure
    :initform nil
    :accessor application-input-controller-failure
    :type (option serious-condition)
    :documentation "A fatal terminal-reader condition awaiting main-thread handling.")
   (failure-backtrace
    :initform nil
    :accessor application-input-controller-failure-backtrace
    :type (option string)
    :documentation "The reader backtrace captured with FAILURE."))
  (:documentation
   "Ephemeral terminal input and FIFO submission state for one application run."))

(-> application--resume-command (application) string)
(defun application--resume-command (application)
  "Return the shell command that resumes APPLICATION's exact conversation."
  (format nil "autolith resume ~A"
          (uiop:escape-shell-token
           (conversation-identifier-display
            (conversation-identifier
             (application-conversation application))))))

(-> application-input-controller--force-interrupt-exit
    (application-input-controller)
    null)
(defun application-input-controller--force-interrupt-exit (controller)
  "Restore the terminal, emit the prepared notice, and force CONTROLLER to exit.

This emergency path deliberately avoids the terminal UI and its presentation
lock because ordinary shutdown may be blocked while either is unavailable."
  (unwind-protect
       (let* ((application
                (application-input-controller-application controller))
              (terminal
                (terminal-ui-terminal (application-ui application))))
         (ignore-errors
           (terminal-stop terminal))
         (ignore-errors
           (terminal--write-safe-text
            terminal
            (application-input-controller-forced-exit-message controller))
           (terminal-flush terminal)))
    (funcall (application-input-controller-forced-exit-function controller)
             *application-forced-interrupt-status*))
  nil)

(-> application-input-controller--forced-exit-message
    (application-input-controller keyword)
    string)
(defun application-input-controller--forced-exit-message (controller reason)
  "Return CONTROLLER's forced-exit notice for shutdown or cancellation REASON.

Forced exit abandons an unfinished run, so every reason carries the durable
resume command that reopens the conversation the run leaves behind."
  (let* ((application
           (application-input-controller-application controller))
         (conversation
           (and (slot-boundp application 'conversation)
                (application-conversation application)))
         (resume-command
           (and conversation
                (conversation-persisted-p conversation)
                (application--resume-command application))))
    (format nil
            "~%~A~%~@[To resume this conversation, run:~%  ~A~%~]"
            (if (eq reason ':turn-cancellation)
                (application-input-controller--forced-exit-text)
                "Ctrl-C pressed during shutdown; forcing Autolith to exit.")
            resume-command)))

(-> application-input-controller--show-interrupt-hint
    (application-input-controller real)
    boolean)
(defun application-input-controller--show-interrupt-hint
    (controller remaining-seconds)
  "Show CONTROLLER's force-exit notice for REMAINING-SECONDS and report display.

The notice expires with the force-exit window itself, and a contended
presentation lock reports no display so a later reader pass can try again."
  (let* ((application
           (application-input-controller-application controller))
         (ui (application-ui application))
         (shown-p nil))
    (ignore-errors
      (setf shown-p
            (nth-value
             1
             (terminal-ui-set-notice
              ui
              (format nil
                      "Press Ctrl-C again within ~A seconds to force exit."
                      (application-input-controller--interrupt-window-text))
              :duration-seconds remaining-seconds))))
    (not (null shown-p))))

(-> application-input-controller--refresh-interrupt-hint
    (application-input-controller)
    null)
(defun application-input-controller--refresh-interrupt-hint (controller)
  "Show CONTROLLER's force-exit hint once cancellation outlives the hint delay.

Waiting keeps a promptly cancelled turn from flashing an option it no longer
offers, so the hint appears only while forcing exit is still worth explaining."
  (let ((hint-time nil)
        (remaining-seconds nil))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((due (application-input-controller-interrupt-hint-time controller))
            (deadline
              (application-input-controller-interrupt-deadline controller)))
        (when (and due deadline)
          (let ((now
                  (funcall
                   (application-input-controller-interrupt-clock-function
                    controller))))
            (when (>= now due)
              (setf hint-time due
                    remaining-seconds (- deadline now)))))))
    (when (and remaining-seconds
               (plusp remaining-seconds)
               (application-input-controller--show-interrupt-hint
                controller remaining-seconds))
      (with-lock-held ((application-input-controller-lock controller))
        ;; A newer press owns any hint time this pass did not observe.
        (when (eql (application-input-controller-interrupt-hint-time controller)
                   hint-time)
          (setf (application-input-controller-interrupt-hint-time controller)
                nil)))))
  nil)

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

(-> application-input-controller--prepare-shutdown
    (application-input-controller keyword)
    (values boolean boolean))
(defun application-input-controller--prepare-shutdown (controller reason)
  "Prepare CONTROLLER shutdown and report active and prepared state.

The first value reports whether a model or command turn needs cancellation. The
second reports whether shutdown was prepared."
  (let ((active-p nil)
        (prepared-p nil)
        (message
          (application-input-controller--forced-exit-message controller reason)))
    (with-lock-held ((application-input-controller-lock controller))
      (setf active-p (application-input-controller-active-p controller))
      (unless (application-input-controller-stopping-p controller)
        (setf prepared-p t)
        (unless (application-input-controller-exit-reason controller)
          (setf (application-input-controller-exit-reason controller) reason
                (application-input-controller-forced-exit-message controller)
                message))
        (when active-p
          (setf (application-input-controller-turn-cancellation-p controller) t
                (application-input-controller-turn-cancellation-delivery-pending-p
                 controller)
                t))
        ;; Persist every accepted input before clearing process-local queues so
        ;; an ordinary restart can restore it for this conversation.
        (application-input-controller--persist-pending controller)
        (mapc #'deque-clear
              (application-input-controller--queues controller))
        (setf (application-input-controller-stopping-p controller) t
              (application-input-controller-active-work controller) nil
              (application-input-controller-active-work-identifier controller) nil
              (application-input-controller-pending-snapshot-identifier controller) nil
              (application-input-controller-vault-capture-identifiers controller) nil
              (application-input-controller-steering-promotion-prefix-count controller) 0
              (application-input-controller-follow-up-edit-index controller) nil
              (application-input-controller-follow-up-edit-work controller) nil)
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller)))
      (values active-p prepared-p))))

(-> application--message-input
    ((or string user-message-input))
    (option (or string user-message-input)))
(defun application--message-input (input)
  "Return INPUT's model message, or NIL for empty, Lisp, or slash input."
  (let ((text (user-message-input-text input)))
    (cond
      ((and (not (non-empty-string-p text))
            (null (user-message-input-image-pathnames input)))
       nil)
      ((uiop:string-prefix-p "//" text)
       (etypecase input
         (string (subseq text 1))
         (user-message-input
          (user-message-input-create
           :text (subseq text 1)
           :image-pathnames (user-message-input-image-pathnames input)))))
      ((terminal-ui--lisp-draft-p text)
       nil)
      ((uiop:string-prefix-p "/" text)
       nil)
      (t
       (user-message-input-copy input)))))

(-> application-input-controller--follow-up-work-p (t) boolean)
(defun application-input-controller--follow-up-work-p (work)
  "Return true when WORK is an editable queued message, command, or Lisp form."
  (and (consp work)
       (member (first work) '(:message :command :lisp) :test #'eq)
       (consp (rest work))
       (typep (second work) '(or string user-message-input))))

(-> application-input-controller--input-work
    ((or string user-message-input))
    (option list))
(defun application-input-controller--input-work (input)
  "Return queued work for INPUT, or NIL when INPUT has no effective content."
  (let ((message (application--message-input input))
        (text (user-message-input-text input)))
    (cond
      ((terminal-ui--lisp-draft-p text)
       (list ':lisp (copy-seq text)))
      (message
       (list ':message message))
      ((non-empty-string-p text)
       (list ':command (copy-seq text)))
      (t
       nil))))

(-> application-input-controller--defer-lisp-submission-p
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--defer-lisp-submission-p (controller input)
  "Continue incomplete Lisp INPUT or reject its image attachments in place."
  (let ((text (user-message-input-text input)))
    (when (terminal-ui--lisp-draft-p text)
      (let* ((application
               (application-input-controller-application controller))
             (ui (application-ui application)))
        (cond
          ((user-message-input-image-pathnames input)
           (application-present
            application
            (list
             (terminal-span ':failure
                            "Local Lisp input cannot include image attachments.")))
           (terminal-ui-set-input ui input)
           t)
          ((application-lisp-input-incomplete-p text)
           (terminal-ui-set-input
            ui
            (application-lisp-input-with-text
             input (concatenate 'string text (string #\Newline))))
           t)
          (t
           nil))))))

(-> application-input-controller--virtual-work-items
    (application-input-controller)
    list)
(defun application-input-controller--virtual-work-items (controller)
  "Return CONTROLLER's FIFO work including its recalled follow-up.

The caller must hold CONTROLLER's lock."
  (let ((work-items
          (deque-copy (application-input-controller-work-items controller)))
        (index
          (application-input-controller-follow-up-edit-index controller))
        (work
          (application-input-controller-follow-up-edit-work controller)))
    (when (and index work)
      (deque-insert work-items (min index (deque-count work-items)) work))
    (deque->list work-items)))

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
            :active-work (application-input-controller-active-work controller)
            :active-work-identifier
            (application-input-controller-active-work-identifier controller)
            :work-items (first queues)
            :steering-items (second queues)
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

(-> application-input--pending-form
    ((or string user-message-input))
    (or string list))
(defun application-input--pending-form (input)
  "Return a portable durable form for pending user INPUT."
  (etypecase input
    (string
     (copy-seq input))
    (user-message-input
     (list :user-message-input
           :version 1
           :text (copy-seq (user-message-input-text input))
           :image-pathnames
           (mapcar #'namestring
                   (user-message-input-image-pathnames input))))))

(-> application-input--restore-pending-form
    (t)
    (option (or string user-message-input)))
(defun application-input--restore-pending-form (form)
  "Return validated pending input restored from durable FORM."
  (cond
    ((stringp form)
     (copy-seq form))
    ((and (listp form)
          (eq (first form) ':user-message-input)
          (= (or (getf (rest form) :version) 0) 1))
     (let ((text (getf (rest form) :text))
           (image-names (getf (rest form) :image-pathnames)))
       (when (and (stringp text)
                  (listp image-names)
                  (every #'stringp image-names))
         (handler-case
             (let ((image-pathnames (mapcar #'pathname image-names)))
               (when (every #'uiop:absolute-pathname-p image-pathnames)
                 (user-message-input-create
                  :text (copy-seq text)
                  :image-pathnames image-pathnames)))
           (error ()
             nil)))))
    (t
     nil)))

(-> application-input-controller--pending-work-entry-form
    (list)
    (option list))
(defun application-input-controller--pending-work-entry-form (work)
  "Return the durable form for one restorable WORK item."
  (when (application-input-controller--follow-up-work-p work)
    (list (first work)
          (application-input--pending-form (second work)))))

(-> application-input-controller--pending-work-form (list) list)
(defun application-input-controller--pending-work-form (work-items)
  "Return durable forms for WORK-ITEMS that can be restored after restart."
  (remove nil
          (mapcar #'application-input-controller--pending-work-entry-form
                  work-items)))

(-> application-input-controller--restore-work-item (t) (option list))
(defun application-input-controller--restore-work-item (form)
  "Return one validated in-memory work item restored from durable FORM."
  (when (and (listp form)
             (member (first form) '(:message :command :lisp) :test #'eq))
    (let ((input (application-input--restore-pending-form (second form))))
      (when input
        (list (first form) input)))))

(-> application-input-controller--restore-work-items (list) list)
(defun application-input-controller--restore-work-items (forms)
  "Return in-memory work items restored from durable FORMS."
  (remove nil
          (mapcar #'application-input-controller--restore-work-item forms)))

(-> application-input-controller--pending-steering-entry-form
    (agent-steering-input)
    list)
(defun application-input-controller--pending-steering-entry-form (entry)
  "Return the durable form for identified in-flight steering ENTRY."
  (list :identifier (copy-seq (agent-steering-input-identifier entry))
        :input (application-input--pending-form
                (agent-steering-input-content entry))))

(-> application-input-controller--restore-steering-entry
    (t)
    (option agent-steering-input))
(defun application-input-controller--restore-steering-entry (form)
  "Return one validated identified steering entry restored from FORM."
  (when (listp form)
    (let ((identifier (getf form :identifier))
          (input (application-input--restore-pending-form (getf form :input))))
      (when (and (non-empty-string-p identifier) input)
        (agent-steering-input-create
         :identifier (copy-seq identifier)
         :content input)))))

(-> application-input-controller--pending-state
    (list string)
    (option list))
(defun application-input-controller--pending-state (form conversation-identifier)
  "Return normalized validated pending state from durable FORM."
  (when (and (listp form)
             (eq (first form) ':pending-inputs)
             (stringp (getf (rest form) :conversation-id))
             (string= (getf (rest form) :conversation-id)
                      conversation-identifier))
    (let ((version (or (getf (rest form) :version) 0)))
      (case version
        (1
         (let* ((steering-forms (getf (rest form) :steering))
                (work-forms (getf (rest form) :work))
                (steering
                  (and (listp steering-forms)
                       (mapcar #'application-input--restore-pending-form
                               steering-forms)))
                (work
                  (and (listp work-forms)
                       (mapcar #'application-input-controller--restore-work-item
                               work-forms))))
           (when (and (listp steering-forms)
                      (listp work-forms)
                      (every #'identity steering)
                      (every #'identity work))
             (list :snapshot-identifier (make-identifier)
                   :active-work nil
                   :active-work-identifier nil
                   :steering-in-flight-items nil
                   :steering-items steering
                   :work-items work
                   :vault-capture-identifiers nil
                   :steering-promotion-prefix-count 0
                   :legacy-p t))))
        (2
         (let* ((snapshot-identifier
                  (getf (rest form) :snapshot-identifier))
                (active-form (getf (rest form) :active-work))
                (active-work
                  (and active-form
                       (application-input-controller--restore-work-item
                        (getf active-form :work))))
                (active-work-identifier
                  (and active-form (getf active-form :identifier)))
                (in-flight-forms (getf (rest form) :steering-in-flight))
                (in-flight
                  (and (listp in-flight-forms)
                       (mapcar
                        #'application-input-controller--restore-steering-entry
                        in-flight-forms)))
                (steering-forms (getf (rest form) :steering))
                (steering
                  (and (listp steering-forms)
                       (mapcar #'application-input--restore-pending-form
                               steering-forms)))
                (work-forms (getf (rest form) :work))
                (work
                  (and (listp work-forms)
                       (mapcar #'application-input-controller--restore-work-item
                               work-forms)))
                (vault-capture-identifiers
                  (or (getf (rest form) :vault-capture-identifiers) nil))
                (steering-promotion-prefix-count
                  (or (getf (rest form) :steering-promotion-prefix-count) 0)))
           (when (and (non-empty-string-p snapshot-identifier)
                      (or (null active-form)
                          (and (non-empty-string-p active-work-identifier)
                               active-work))
                      (listp in-flight-forms)
                      (every #'identity in-flight)
                      (listp steering-forms)
                      (every #'identity steering)
                      (listp work-forms)
                      (every #'identity work)
                      (listp vault-capture-identifiers)
                      (every #'non-empty-string-p vault-capture-identifiers)
                      (= (length vault-capture-identifiers)
                         (length
                          (remove-duplicates vault-capture-identifiers
                                             :test #'string=)))
                      (typep steering-promotion-prefix-count '(integer 0))
                      (<= steering-promotion-prefix-count (length work)))
             (list :snapshot-identifier (copy-seq snapshot-identifier)
                   :active-work active-work
                   :active-work-identifier
                   (and active-work-identifier
                        (copy-seq active-work-identifier))
                   :steering-in-flight-items in-flight
                   :steering-items steering
                   :work-items work
                   :vault-capture-identifiers
                   (mapcar #'copy-seq vault-capture-identifiers)
                   :steering-promotion-prefix-count
                   steering-promotion-prefix-count
                   :legacy-p nil))))
        (otherwise
         nil)))))

(-> application-input-controller--pending-state-input-count (list) (integer 0))
(defun application-input-controller--pending-state-input-count (state)
  "Return the number of accepted user inputs represented by pending STATE."
  (+ (if (getf state :active-work) 1 0)
     (length (getf state :steering-in-flight-items))
     (length (getf state :steering-items))
     (length (getf state :work-items))))

(-> application-input-controller--pending-state-filter-persisted
    (list conversation)
    list)
(defun application-input-controller--pending-state-filter-persisted
    (state conversation)
  "Return STATE after removing input already durable in CONVERSATION."
  (let* ((persisted-identifiers
           (conversation-pending-input-identifiers conversation))
         (active-work (getf state :active-work))
         (active-work-identifier (getf state :active-work-identifier))
         (active-survives-p
           (and active-work
                (not
                 (member active-work-identifier
                         persisted-identifiers
                         :test #'string=))))
         (in-flight
           (remove-if
            (lambda (entry)
              (member (agent-steering-input-identifier entry)
                      persisted-identifiers
                      :test #'string=))
            (getf state :steering-in-flight-items)))
         (steering (getf state :steering-items))
         (work (getf state :work-items))
         (steering-promotion-prefix-count
           (min (getf state :steering-promotion-prefix-count)
                (length work)))
         (promoted-work
           (append
            (mapcar (lambda (entry)
                      (list ':message (agent-steering-input-content entry)))
                    in-flight)
            (mapcar (lambda (input)
                      (list ':message input))
                    steering))))
    (list :snapshot-identifier (getf state :snapshot-identifier)
          :active-work (and active-survives-p active-work)
          :active-work-identifier
          (and active-survives-p active-work-identifier)
          :steering-in-flight-items (and active-survives-p in-flight)
          :steering-items (and active-survives-p steering)
          :work-items
          (if active-survives-p
              work
              (append (subseq work 0 steering-promotion-prefix-count)
                      promoted-work
                      (nthcdr steering-promotion-prefix-count work)))
          :vault-capture-identifiers
          (getf state :vault-capture-identifiers)
          :steering-promotion-prefix-count
          (if active-survives-p steering-promotion-prefix-count 0)
          :legacy-p (getf state :legacy-p))))

(-> application-input-controller--pending-state-form (list string) list)
(defun application-input-controller--pending-state-form
    (state conversation-identifier)
  "Return the version-two durable form for normalized pending STATE."
  (let ((active-work (getf state :active-work))
        (active-work-identifier (getf state :active-work-identifier)))
    (list :pending-inputs
          :version 2
          :snapshot-identifier (getf state :snapshot-identifier)
          :conversation-id conversation-identifier
          :active-work
          (and active-work
               active-work-identifier
               (list :identifier active-work-identifier
                     :work
                     (application-input-controller--pending-work-entry-form
                      active-work)))
          :steering-in-flight
          (mapcar #'application-input-controller--pending-steering-entry-form
                  (getf state :steering-in-flight-items))
          :steering
          (mapcar #'application-input--pending-form
                  (getf state :steering-items))
          :work
          (application-input-controller--pending-work-form
           (getf state :work-items))
          :vault-capture-identifiers
          (mapcar #'copy-seq
                  (getf state :vault-capture-identifiers))
          :steering-promotion-prefix-count
          (or (getf state :steering-promotion-prefix-count) 0))))

(-> application-input-controller--persist-pending
    (application-input-controller &key (:error-p boolean) (:sync-vault-p boolean))
    boolean)
(defun application-input-controller--persist-pending
    (controller &key (error-p nil) (sync-vault-p t))
  "Atomically publish CONTROLLER's accepted but unprocessed input."
  (block nil
    (unless (application-input-controller-pending-persistence-enabled-p controller)
      (return nil))
    (let* ((application (application-input-controller-application controller))
           (configuration
             (and (slot-boundp application 'configuration)
                  (application-configuration application)))
           (conversation
             (and (slot-boundp application 'conversation)
                  (application-conversation application)))
           (old-snapshot-identifier
             (application-input-controller-pending-snapshot-identifier
              controller)))
      (unless (and (typep configuration 'configuration)
                   (typep conversation 'conversation))
        (return nil))
      (handler-case
          (let* ((pathname
                   (configuration-pending-inputs-path
                    configuration (conversation-pathname conversation)))
                 (state
                  (application-input-controller--state controller))
                 (work
                   (application-input-controller--virtual-work-items controller))
                 (pending-p
                   (or (getf state :active-work)
                       (getf state :steering-in-flight-items)
                       (getf state :steering-items)
                       work)))
            (setf (getf state :work-items) work)
            (if (null pending-p)
                (progn
                  (when (probe-file pathname)
                    (delete-file pathname))
                  (setf
                   (application-input-controller-pending-snapshot-identifier
                    controller)
                   nil
                   (application-input-controller-vault-capture-identifiers
                    controller)
                   nil
                   (application-input-controller-steering-promotion-prefix-count
                    controller)
                   0)
                  (when (and sync-vault-p
                             (non-empty-string-p old-snapshot-identifier))
                    (application-recovery-input-vault--replace-capture
                     application old-snapshot-identifier nil)))
                (let ((snapshot-identifier
                        (or
                         (getf state :snapshot-identifier)
                         (setf
                          (application-input-controller-pending-snapshot-identifier
                           controller)
                          (make-identifier)))))
                  (setf (getf state :snapshot-identifier) snapshot-identifier)
                  (ensure-directories-exist pathname)
                  (snapshot-write
                   pathname
                   (application-input-controller--pending-state-form
                    state (conversation-identifier conversation))
                   :mode #o600)
                  (when sync-vault-p
                    (application-recovery-input-vault-sync-pending
                     application state
                     :create-p
                     (application-input-controller-live-vault-sync-p
                      controller)))))
            t)
        (error (condition)
          (when error-p
            (error condition))
          nil)))))

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
                 ((probe-file pathname) pathname)
                 ((probe-file legacy-pathname) legacy-pathname)
                 (t nil))))
        (when source-pathname
          (handler-case
              (multiple-value-bind (form complete-p)
                  (snapshot-read source-pathname)
                (let* ((state
                         (and complete-p
                              (application-input-controller--pending-state
                               form (conversation-identifier conversation))))
                       (filtered-state
                         (and state
                              (application-input-controller--pending-state-filter-persisted
                               state conversation))))
                  (when filtered-state
                    (let* ((active-work (getf filtered-state :active-work))
                           (steering
                             (application-input-controller-steering-items
                              controller))
                           (work
                             (application-input-controller-work-items controller)))
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
                          (application-input-controller--persist-pending
                           controller :sync-vault-p nil))
                      (when (equal source-pathname legacy-pathname)
                        (when (probe-file pathname)
                          (delete-file legacy-pathname)))))))
            (error ()
              nil))))))
  nil)

(-> application-input-controller--publish-counts
    (application-input-controller)
    null)
(defun application-input-controller--publish-counts (controller)
  "Publish CONTROLLER's pending input previews through its serialized UI.

A stopping controller has already finalized its durable pending-input snapshot.
Skipping a later publisher prevents an accepted enqueue from deleting that
snapshot after shutdown cleared the process-local queues."
  (with-lock-held ((application-input-controller-lock controller))
    (terminal-ui-set-pending-inputs
     (application-ui (application-input-controller-application controller))
     (mapcar #'user-message-input-text
             (deque->list
              (application-input-controller-steering-items controller)))
     (loop for work in
           (deque->list (application-input-controller-work-items controller))
           for input = (second work)
           when (typep input '(or string user-message-input))
             collect (user-message-input-text input)))
    (unless (application-input-controller-stopping-p controller)
      (application-input-controller--persist-pending controller)))
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
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (or (application-input-controller-active-p controller)
          (find-if-not #'deque-empty-p
                       (application-input-controller--queues controller))
          (application-input-controller-follow-up-edit-work controller))))))

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
  (let ((thread (application-input-controller-main-thread controller)))
    (unless (eq thread (current-thread))
      (when (thread-alive-p thread)
        (interrupt-thread thread (lambda () (error condition))))))
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
  "Promptly deliver CONTROLLER's pending turn cancellation to its main thread."
  (let ((thread (application-input-controller-main-thread controller)))
    (unless (eq thread (current-thread))
      (when (thread-alive-p thread)
        (interrupt-thread
         thread
         (lambda ()
           (when
               (application-input-controller--consume-turn-cancellation-delivery-p
                controller)
             (error (make-condition 'application-turn-cancelled))))))))
  nil)

(-> application-input-controller--record-failure
    (application-input-controller serious-condition (option string))
    null)
(defun application-input-controller--record-failure
    (controller condition backtrace)
  "Record reader CONDITION, preserve pending input, and wake the main thread."
  (let ((active-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-failure controller)
        (setf (application-input-controller-failure controller) condition
              (application-input-controller-failure-backtrace controller) backtrace
              (application-input-controller-stopping-p controller) t)
        ;; A reader failure is a fatal process failure. Publish every accepted input
        ;; before later stopping-state publishers begin skipping persistence.
        (application-input-controller--persist-pending controller))
      (setf active-p (application-input-controller-active-p controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition
            'application-input-failed
            :original-condition condition
            :backtrace backtrace))
        (error ()
          nil))))
  nil)

(-> application-input-controller--enqueue
    (application-input-controller keyword (or string user-message-input))
    boolean)
(defun application-input-controller--enqueue (controller kind input)
  "Append one work item of KIND carrying INPUT and report acceptance."
  (let ((queued-p nil)
        (resumed-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (or (application-input-controller-stopping-p controller)
                  (application-input-controller-localgroup-handoff-p controller))
        (setf resumed-p
              (application-input-controller-queued-work-paused-p controller)
              (application-input-controller-queued-work-paused-p controller) nil
              queued-p t
              (application-input-controller-live-vault-sync-p controller) t)
        (deque-push-back
         (application-input-controller-work-items controller)
         (list kind (user-message-input-copy input)))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller))))
    (when resumed-p
      (terminal-ui-set-notice
       (application-ui (application-input-controller-application controller))
       nil))
    (application-input-controller--publish-counts controller)
    queued-p))

(-> application-input-controller--queue-input
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--queue-input (controller input)
  "Queue INPUT, restoring a recalled follow-up to its virtual FIFO position."
  (let ((work (application-input-controller--input-work input))
        (queued-p nil)
        (resumed-p nil))
    (when work
      (with-lock-held ((application-input-controller-lock controller))
        (unless (or (application-input-controller-stopping-p controller)
                    (application-input-controller-localgroup-handoff-p controller))
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
         "Recovered input storage is unavailable. Use /vault to inspect it or /vault-discard to discard the preserved input before submitting more work.")
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
  (when (or (application-input-controller-stopping-p controller)
            (application-input-controller-localgroup-handoff-p controller))
    (return-from application-input-controller--admit-primary-prompt-locked nil))
  (let ((copied-input (user-message-input-copy input))
        (work (application-input-controller-work-items controller)))
    (cond
      ((and prefer-steering-p
            (eq (application-input-controller-active-work-kind controller)
                ':message))
       (deque-push-back
        (application-input-controller-steering-items controller)
        copied-input)
       ':steering)
      (queue-index
       (deque-insert work
                     (min queue-index (deque-count work))
                     (list ':message copied-input))
       ':queued)
      ((and prefer-steering-p
            (not (application-input-controller-active-p controller)))
       (let ((index
               (min
                (application-input-controller-steering-promotion-prefix-count
                 controller)
                (deque-count work))))
         (deque-insert work index (list ':message copied-input))
         (setf (application-input-controller-steering-promotion-prefix-count
                controller)
               (1+ index))
         (when (application-input-controller-follow-up-edit-index controller)
           (incf (application-input-controller-follow-up-edit-index controller))))
       ':queued)
      (t
       (deque-push-back work (list ':message copied-input))
       ':queued))))

(-> application-input-controller-submit-primary-prompt
    (application-input-controller (or string user-message-input)
     &key (:prefer-steering-p boolean))
    (values boolean (member :queued :rejected :steering)))
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
  (let ((delivery nil)
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
      (terminal-ui-set-notice
       (application-ui (application-input-controller-application controller))
       nil))
    (application-input-controller--publish-counts controller)
    (values (not (null delivery)) (or delivery ':rejected))))

(-> application-prompt--primary-target-p (string) boolean)
(defun application-prompt--primary-target-p (target)
  "Return whether TARGET explicitly names the primary agent."
  (string-equal target "autolith"))

(-> application-prompt--primary-rejection-reason
    (application-input-controller)
    keyword)
(defun application-prompt--primary-rejection-reason (controller)
  "Return the stable reason CONTROLLER rejected a validated primary prompt."
  (cond
    ((not (application-input-controller-pending-persistence-enabled-p controller))
     ':storage-unavailable)
    (t
     (with-lock-held ((application-input-controller-lock controller))
       (cond
         ((application-input-controller-stopping-p controller)
          ':stopping)
         ((application-input-controller-localgroup-handoff-p controller)
          ':handoff)
         (t
          ':unavailable))))))

(-> application-prompt--visible-child-jobs (application) list)
(defun application-prompt--visible-child-jobs (application)
  "Return task jobs visible to APPLICATION's primary agent."
  (let ((orchestrator (application--task-orchestrator application))
        (viewer
          (and (slot-boundp application 'agent)
               (application-agent application))))
    (unless (and (typep orchestrator 'task-orchestrator)
                 (typep viewer 'agent))
      (prompt--error
       ':task-runtime-unavailable
       "No task runtime is available for named child prompting."))
    (remove-if-not
     (lambda (job) (typep job 'task-job))
     (task-orchestrator-list-visible-jobs orchestrator viewer))))

(-> application-prompt--find-child (application string) task-job)
(defun application-prompt--find-child (application target)
  "Return TARGET's unique visible child, matching display name before job ID."
  (let* ((jobs (application-prompt--visible-child-jobs application))
         (name-matches
           (remove-if-not
            (lambda (job)
              (string-equal target (task-job-display-name job)))
            jobs)))
    (cond
      ((> (length name-matches) 1)
       (prompt--error
        ':ambiguous-target
        (format nil "More than one visible child is named ~S." target)
        :target target))
      ((first name-matches)
       (first name-matches))
      (t
       (let ((identifier-matches
               (remove-if-not
                (lambda (job)
                  (string-equal target (session-job-identifier job)))
                jobs)))
         (cond
           ((> (length identifier-matches) 1)
            (prompt--error
             ':ambiguous-target
             (format nil "More than one visible child has job ID ~S." target)
             :target target))
           ((first identifier-matches)
            (first identifier-matches))
           (t
            (prompt--error
             ':unknown-target
             (format nil "No visible child is named ~S." target)
             :target target))))))))

(-> application-prompt--child-admission-error
    (task-job string keyword)
    nil)
(defun application-prompt--child-admission-error (job target reason)
  "Signal a typed prompt failure for JOB's rejected steering REASON."
  (let* ((snapshot (task-job-snapshot job))
         (state (getf snapshot :state))
         (cancellation-reason (getf snapshot :cancellation-reason))
         (name (task-job-display-name job)))
    (cond
      (cancellation-reason
       (prompt--error
        ':cancelled
        (format nil "Child ~A is cancelling or cancelled." name)
        :target target))
      ((eq state ':queued)
       (prompt--error
        ':not-running
        (format nil "Child ~A is queued and not yet running." name)
        :target target))
      ((task--terminal-state-p state)
       (prompt--error
        ':terminal
        (format nil "Child ~A has already reached terminal state ~S." name state)
        :target target))
      ((eq reason ':full)
       (prompt--error
        ':full
        (format nil "Child ~A cannot retain another prompt yet." name)
        :target target))
      ((eq reason ':content-too-large)
       (prompt--error
        ':content-too-large
        (format nil "Prompt content exceeds child ~A's steering limit." name)
        :target target))
      ((eq reason ':closing)
       (prompt--error
        ':closing
        (format nil "Child ~A is closing and cannot accept another prompt." name)
        :target target))
      ((eq reason ':closed)
       (prompt--error
        ':closed
        (format nil "Child ~A has closed prompt admission." name)
        :target target))
      (t
       (prompt--error
        ':not-running
        (format nil "Child ~A is not accepting prompts." name)
        :target target)))))

(defmethod application-submit-prompt
    ((application application) target input)
  "Prompt APPLICATION's primary agent or steer one visible running child."
  (if (application-prompt--primary-target-p target)
      (let ((controller
              (and (slot-boundp application 'input-controller)
                   (application-input-controller application))))
        (unless (typep controller 'application-input-controller)
          (prompt--error
           ':controller-unavailable
           "Primary Autolith has no active input controller."
           :target target))
        (application-localgroup-resume application)
        (multiple-value-bind (accepted-p delivery)
            (application-input-controller-submit-primary-prompt
             controller
             input
             :prefer-steering-p *prompt-primary-prefer-steering-p*)
          (unless accepted-p
            (let ((reason
                    (application-prompt--primary-rejection-reason controller)))
              (prompt--error
               reason
               (format nil "Primary Autolith is not accepting prompts (~(~A~))."
                       reason)
               :target target)))
          (list :prompt
                :accepted-p t
                :target ':autolith
                :delivery delivery
                :content-characters (length (user-message-input-text input))
                :image-count (length
                              (user-message-input-image-pathnames input)))))
        (let* ((controller (application-input-controller application))
               (job (application-prompt--find-child application target))
               (snapshot (task-job-snapshot job)))
          (unless (and controller
                       (application-input-controller--prompt-storage-ready-p
                        controller))
            (prompt--error
             ':storage-unavailable
             "Recovered input storage is unavailable. Use /vault to inspect it or /vault-discard to discard the preserved input before submitting more work."
             :target target))
          (unless (eq (getf snapshot :state) ':running)
            (application-prompt--child-admission-error
             job target ':not-running))
          (application-recovery-input-vault-capture-message application input)
          (multiple-value-bind (entry reason)
              (task-job-enqueue-steering
               job input :promote-response-p t)
            (unless entry
              (application-prompt--child-admission-error job target reason))
            (list :prompt
                  :accepted-p t
                  :target ':child
                  :child-name (task-job-display-name job)
                  :job-id (session-job-identifier job)
                  :execution-id (task-job-execution-identifier job)
                  :steering-id (agent-steering-input-identifier entry)
                  :delivery ':steering
                  :content-characters (length (user-message-input-text input))
                  :image-count (length
                                (user-message-input-image-pathnames input)))))))


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
             :key (lambda (input)
                    (agent-steering-input-create
                     :identifier (make-identifier)
                     :content input)))
          (declare (ignore ignored-target ignored-evicted))
          (setf entries (coerce moved 'list)))
        ;; The old snapshot still contains queued steering if this atomic
        ;; replacement fails, while the new snapshot names every in-flight item.
        (application-input-controller--persist-pending controller)))
    (application-input-controller--publish-counts controller)
    entries))

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
              acknowledged-p t)
        (application-input-controller--persist-pending controller)))
    acknowledged-p))

(-> application-input-controller--acknowledge-steering
    (application-input-controller non-empty-string)
    boolean)
(defun application-input-controller--acknowledge-steering (controller identifier)
  "Forget exactly one in-flight steering IDENTIFIER after durable append."
  (let ((acknowledged-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (multiple-value-bind (entry present-p)
          (deque-delete
           identifier
           (application-input-controller-steering-in-flight-items controller)
           :key #'agent-steering-input-identifier
           :test #'string=)
        (declare (ignore entry))
        (when present-p
          (setf acknowledged-p t)
          (application-input-controller--persist-pending controller))))
    acknowledged-p))

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
     &key (:force-exit-window-p boolean)
          (:pause-queued-work-p boolean))
    boolean)
(defun application-input-controller--request-active-turn-cancellation
    (controller &key force-exit-window-p pause-queued-work-p)
  "Atomically request cancellation only for CONTROLLER's current active turn.

When FORCE-EXIT-WINDOW-P is true, arm the repeated-Ctrl-C option before
cancellation can finish and schedule the hint that explains it. When
PAUSE-QUEUED-WORK-P is true, hold follow-ups after cancellation until explicit
new input resumes them."
  (let ((accepted-p nil)
        (message
          (and force-exit-window-p
               (application-input-controller--forced-exit-message
                controller ':turn-cancellation))))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (not (application-input-controller-stopping-p controller))
                 (not
                  (application-input-controller-turn-cancellation-p controller)))
        (setf (application-input-controller-turn-cancellation-p controller) t
              (application-input-controller-turn-cancellation-delivery-pending-p
               controller)
              t
              (application-input-controller-queued-work-paused-p controller)
              pause-queued-work-p
              accepted-p t)
        (when force-exit-window-p
          (let ((now
                  (funcall
                   (application-input-controller-interrupt-clock-function
                    controller))))
            (setf (application-input-controller-interrupt-deadline controller)
                  (+ now *application-interrupt-exit-window-seconds*)
                  (application-input-controller-interrupt-hint-time controller)
                  (+ now *application-interrupt-hint-delay-seconds*)
                  (application-input-controller-forced-exit-message controller)
                  message)))))
    (when accepted-p
      (handler-case
          (application-input-controller--interrupt-main-for-turn-cancellation
           controller)
        (error ()
          nil)))
    accepted-p))

(-> application-input-controller--active-turn-interrupt-action
    (application-input-controller)
    (option keyword))
(defun application-input-controller--active-turn-interrupt-action (controller)
  "Return and apply CONTROLLER's active-cancellation Ctrl-C action, if any.

The forced-exit notice is prepared outside the lock because a lapsed press
re-arms a window that a wedged turn may never let ordinary shutdown reach."
  (let ((message
          (application-input-controller--forced-exit-message
           controller ':turn-cancellation)))
    (with-lock-held ((application-input-controller-lock controller))
      (when (application-input-controller-turn-cancellation-p controller)
        (let* ((now
                 (funcall
                  (application-input-controller-interrupt-clock-function
                   controller)))
               (deadline
                 (application-input-controller-interrupt-deadline controller)))
          (if (and deadline (<= now deadline))
              (progn
                (setf (application-input-controller-interrupt-deadline controller)
                      nil
                      (application-input-controller-interrupt-hint-time controller)
                      nil)
                ':force)
              (progn
                ;; This press already outlived one window, so the wedged turn
                ;; has earned its hint at the reader's next opportunity.
                (setf (application-input-controller-interrupt-deadline controller)
                      (+ now *application-interrupt-exit-window-seconds*)
                      (application-input-controller-interrupt-hint-time controller)
                      now
                      (application-input-controller-forced-exit-message controller)
                      message)
                ':hint)))))))

(-> application-input-controller--present-scheduled-command
    (application-input-controller string)
    null)
(defun application-input-controller--present-scheduled-command (controller input)
  "Present the busy-command scheduling result for INPUT."
  (let ((invocation (application-command-invocation-parse input)))
    (application-command--call-with-presentation
     invocation
     (lambda ()
       (application-present
        (application-input-controller-application controller)
        (list
         (terminal-span
          ':hint
          "∙ command scheduled until the current response finishes")
         (terminal-span ':plain (string #\Newline))
         (terminal-span
          ':dim
          "  It runs at the first opportunity. Empty Tab edits it; Shift-Tab cycles."))))))
  nil)

(-> application-input-controller--schedule-command
    (application-input-controller string)
    null)
(defun application-input-controller--schedule-command (controller input)
  "Queue busy command INPUT to run at the first idle opportunity."
  (when (application-input-controller--enqueue controller ':command input)
    (application-input-controller--present-scheduled-command controller input))
  nil)

(-> application-input-controller--present-scheduled-lisp
    (application-input-controller string)
    null)
(defun application-input-controller--present-scheduled-lisp (controller source)
  "Present the active-turn scheduling result for explicit local Lisp SOURCE."
  (application-present
   (application-input-controller-application controller)
   (list
    (terminal-span ':hint
                   "∙ local evaluation scheduled until the current response finishes")
    (terminal-span ':plain (string #\Newline))
    (terminal-span ':dim
                   (format nil "  ~A"
                           (text-cell-prefix
                            (sanitize-text source :single-line-p t)
                            72)))))
  nil)

(-> application-input-controller--schedule-lisp
    (application-input-controller string)
    null)
(defun application-input-controller--schedule-lisp (controller source)
  "Queue local Lisp SOURCE to run at the first idle opportunity."
  (when (application-input-controller--enqueue controller ':lisp source)
    (application-input-controller--present-scheduled-lisp controller source))
  nil)

(-> application-input-controller--schedule-apply
    (application-input-controller string)
    null)
(defun application-input-controller--schedule-apply (controller input)
  "Queue busy command INPUT to apply at the active turn's next safe boundary."
  (with-lock-held ((application-input-controller-lock controller))
    (deque-push-back
     (application-input-controller-pending-apply-items controller)
     input))
  (application-present
   (application-input-controller-application controller)
   (list
    (terminal-span ':hint
                   "∙ command applies at the next safe point in the active response")
    (terminal-span ':plain (string #\Newline))
    (terminal-span ':dim
                   (format nil "  ~A"
                           (text-cell-prefix
                            (sanitize-text input :single-line-p t)
                            72)))))
  nil)

(-> application-input-controller--take-pending-applies
    (application-input-controller)
    list)
(defun application-input-controller--take-pending-applies (controller)
  "Drain and return CONTROLLER's queued apply-at-boundary command inputs."
  (with-lock-held ((application-input-controller-lock controller))
    (let ((items (application-input-controller-pending-apply-items controller))
          (drained nil))
      (loop until (deque-empty-p items)
            do (push (deque-pop-front items) drained))
      (nreverse drained))))

(-> application-input-controller--apply-pending-commands
    (application-input-controller &key (:agent t))
    null)
(defun application-input-controller--apply-pending-commands (controller &key agent)
  "Apply queued commands now, then let running AGENT adopt replaced runtime.

Runs non-interactively so a boundary application never opens terminal
pickers. AGENT, when supplied, is the loop still executing the current turn;
it adopts a replaced configuration, provider, or tool registry so the change
reaches the very next provider request."
  (let ((inputs (application-input-controller--take-pending-applies controller))
        (application (application-input-controller-application controller)))
    (when inputs
      (dolist (input inputs)
        (let ((*application-command-interactive-p* nil))
          (application--run-command-input application input)))
      (when agent
        (application--agent-adopt-runtime application agent))))
  nil)

(-> application-input-controller--lisp-active-turn-action
    (application-input-controller string)
    (member :cancel :execute :hold))
(defun application-input-controller--lisp-active-turn-action (controller source)
  "Return SOURCE's admission action when CONTROLLER has an active turn."
  (if (application-input-controller-turn-active-p controller)
      (application-operation-source-active-turn-action
       (application-input-controller-application controller) source)
      ':hold))

(-> application-input-controller--prompt-result-failure-p (t) boolean)
(defun application-input-controller--prompt-result-failure-p (result)
  "Return true when RESULT denotes a failed user interaction."
  (not (null (member result '(:aborted :failed :rate-limited) :test #'eq))))

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
         (status 0)
         (reopen-p t)
         (result nil)
         (result-p nil))
    (application-input-controller--open-prompt-if-ready controller)
    (setf marker-p (terminal-ui-start-prompt-execution ui))
    (unwind-protect
         (handler-case
             (progn
               (setf result (funcall function)
                     result-p t)
               (when (application-input-controller--prompt-result-failure-p
                      result)
                 (setf status 1))
               result)
           (serious-condition (condition)
             (setf status 1
                   reopen-p (typep condition 'application-turn-cancelled))
             (error condition)))
      (when marker-p
        (terminal-ui-finish-prompt-block ui status)
        (when (and reopen-p
                   (not (and result-p (eq result ':quit))))
          (application-input-controller--open-prompt-if-ready controller))))))

(-> application--call-with-command-debugger
    (application function &key (:expected-error-function function))
    (values keyword (option string)))
(defun application--call-with-command-debugger
    (application function
     &key (expected-error-function #'application-handle-expected-error))
  "Call FUNCTION under the user restart debugger and fatal condition boundary."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (multiple-value-bind
                (values debugger-status condition restart-names
                        selected-restart-name)
              (application-lisp-call-with-ui-debugger
               application function
               :debug-condition-p
               (lambda (condition)
                 (not (typep condition 'autolith-error))))
            (declare (ignore restart-names selected-restart-name))
            (if (eq debugger-status ':aborted)
                (values ':aborted condition)
                (values (first values) condition)))
        ((or application-operation-loop-action
             application-turn-cancelled
             application-input-failed
             rollback-requested
             update-requested)
         (condition)
          (error condition))
        ((or agent-loop-error
             conversation-invariant-error
             active-image-corruption)
         (condition)
          (application-raise-fatal application condition signal-backtrace))
        (autolith-error (condition)
          (funcall expected-error-function application condition)
          (values ':failed (princ-to-string condition)))
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application-input-controller--run-responsive-lisp
    (application-input-controller string)
    keyword)
(defun application-input-controller--run-responsive-lisp (controller source)
  "Execute immediate local Lisp SOURCE on CONTROLLER's terminal reader."
  (application-input-controller--call-with-responsive-prompt-block
   controller
   (lambda ()
     (application-run-lisp-input
      (application-input-controller-application controller) source))))

(-> application-input-controller--run-responsive-unknown
    (application-input-controller string)
    keyword)
(defun application-input-controller--run-responsive-unknown (controller input)
  "Report unknown or malformed command INPUT immediately during active work."
  (application-input-controller--call-with-responsive-prompt-block
   controller
   (lambda ()
     (application--run-command-input
      (application-input-controller-application controller)
      input))))

(-> application-input-controller--run-responsive-command
    (application-input-controller application-command
     application-command-invocation)
    keyword)
(defun application-input-controller--run-responsive-command
    (controller command invocation)
  "Execute an immediate COMMAND through the local restart debugger."
  (application-input-controller--call-with-responsive-prompt-block
   controller
   (lambda ()
     (let ((application
             (application-input-controller-application controller))
           (*application-command-interactive-p* t))
        (application-command--call-with-presentation
         invocation
         (lambda ()
           (multiple-value-bind (result condition)
               (application--call-with-command-debugger
                application
                (lambda ()
                  (application-command-execute command application invocation))
                :expected-error-function
                (lambda (observed-application observed-condition)
                  (application-present
                   observed-application
                   (application--expected-error-entry
                    observed-application observed-condition))))
             (application-user-operation-record-command-outcome
              application invocation :action result :condition condition)
             result)))))))

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
         (message (application--message-input input))
         (text (user-message-input-text input))
         (lisp-input-p (terminal-ui--lisp-draft-p text))
         (work (application-input-controller--input-work input))
         (invocation
           (and (null message)
                (not lisp-input-p)
                (non-empty-string-p text)
                (application-command-invocation-parse text)))
         (command
           (and invocation
                (application-command-invocation-command invocation)))
         (busy-action
           (cond
             (lisp-input-p
              (application-input-controller--lisp-active-turn-action
               controller text))
             (invocation
              (if command
                  (application-command-busy-action command invocation)
                  ;; An unknown command can only ever error, so report it
                  ;; immediately instead of scheduling the mistake.
                  ':execute))))
         (handled-p nil)
         (changed-p nil)
         (post-action nil))
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
              (message
               (let ((delivery
                        (application-input-controller--admit-primary-prompt-locked
                         controller
                         message
                         :prefer-steering-p prefer-steering-p
                         :queue-index index)))
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
      (:cancel
       (application-input-controller--request-exit controller ':quit))
      (:execute
       (let ((result
               (cond
                 (lisp-input-p
                  (application-input-controller--run-responsive-lisp
                   controller text))
                 (command
                  (application-input-controller--run-responsive-command
                   controller command invocation))
                 (t
                  (application-input-controller--run-responsive-unknown
                   controller text)))))
         (when (eq result ':quit)
           (application-input-controller--request-exit controller ':quit))))
      (:apply
       (application-input-controller--schedule-apply controller text))
      (:hold
       (if lisp-input-p
           (application-input-controller--present-scheduled-lisp controller text)
           (application-input-controller--present-scheduled-command controller text))))
    handled-p))

(-> application-input-controller--vault-command-p
    ((or string user-message-input))
    boolean)
(defun application-input-controller--vault-command-p (input)
  "Return true when INPUT is one of the recovery-vault control commands."
  (let ((text (user-message-input-text input)))
    (not
     (null
      (member text
              '("/vault" "/vault-restore" "/vault-discard")
              :test #'string=)))))

(-> application-input-controller--submission-storage-ready-p
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--submission-storage-ready-p (controller input)
  "Return true when INPUT may be accepted without replacing preserved state."
  (or (application-input-controller-pending-persistence-enabled-p controller)
      (application-input-controller--vault-command-p input)
      (application-input-controller--prompt-storage-ready-p controller)))

(-> application-input-controller--prompt
    (application-input-controller (or string user-message-input)
     &key (:prefer-steering-p boolean))
    boolean)
(defun application-input-controller--prompt
    (controller input &key (prefer-steering-p t))
  "Submit terminal INPUT through canonical PROMPT and report acceptance."
  (let* ((application
           (application-input-controller-application controller))
         (*application-operation-application* application)
         (*prompt-primary-prefer-steering-p* prefer-steering-p))
    (handler-case
        (progn
          (prompt input)
          t)
      (prompt-error (condition)
        (unless (eq (prompt-error-reason condition) ':storage-unavailable)
          (application-present application (autolith-error-message condition)))
        nil))))

(-> application-input-controller--handle-submission
    (application-input-controller (or string user-message-input)
     &key (:steer-p boolean))
    null)
(defun application-input-controller--handle-submission
    (controller input &key steer-p)
  "Route submitted INPUT to canonical prompt, local Lisp, or command policy."
  (let* ((application
           (application-input-controller-application controller))
         (message (application--message-input input))
         (text (user-message-input-text input)))
    (cond
      (message
       (application-input-controller--prompt
        controller message :prefer-steering-p steer-p))
      ((not (application-input-controller--submission-storage-ready-p
             controller input))
       nil)
      (t
       (application-localgroup-resume application)
       (cond
         ((terminal-ui--lisp-draft-p text)
          (if (application-input-controller-busy-p controller)
              (ecase (application-input-controller--lisp-active-turn-action
                      controller text)
                (:cancel
                 (application-input-controller--request-exit controller ':quit))
                (:execute
                 (when (eq (application-input-controller--run-responsive-lisp
                            controller text)
                           ':quit)
                   (application-input-controller--request-exit controller ':quit)))
                (:hold
                 (application-input-controller--schedule-lisp controller text)))
              (application-input-controller--enqueue controller ':lisp text)))
         ((not (non-empty-string-p text))
          nil)
         ((application-input-controller-busy-p controller)
          (let* ((invocation (application-command-invocation-parse text))
                 (command
                   (application-command-invocation-command invocation))
                 (action
                   (if command
                       (application-command-busy-action command invocation)
                       ;; An unknown command can only ever error, so report
                       ;; it immediately instead of scheduling the mistake.
                       ':execute)))
            (ecase action
              (:cancel
               (application-input-controller--request-exit controller ':quit))
              (:execute
               (when (eq (if command
                             (application-input-controller--run-responsive-command
                              controller command invocation)
                             (application-input-controller--run-responsive-unknown
                              controller text))
                         ':quit)
                 (application-input-controller--request-exit controller ':quit)))
              (:apply
               (application-input-controller--schedule-apply controller text))
              (:hold
               (application-input-controller--schedule-command controller text)))))
         (t
          (application-input-controller--enqueue controller ':command text))))))
  nil)

(-> application-input-controller--handle-queue-submission
    (application-input-controller (or string user-message-input))
    null)
(defun application-input-controller--handle-queue-submission (controller input)
  "Queue terminal INPUT as post-turn work through canonical prompt when prose."
  (let ((message (application--message-input input)))
    (if message
        (application-input-controller--prompt
         controller message :prefer-steering-p nil)
        (when (application-input-controller--submission-storage-ready-p
               controller input)
          (application-input-controller--queue-input controller input))))
  nil)

(-> application-input-controller--recall-follow-up
    (application-input-controller)
    boolean)
(defun application-input-controller--recall-follow-up (controller)
  "Recall CONTROLLER's newest follow-up into the editor for revision."
  (let ((work nil)
        (steering-inputs nil)
        (queued-inputs nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (null
                  (application-input-controller-follow-up-edit-index controller)))
        (let* ((work-items
                 (application-input-controller-work-items controller))
               (index
                 (deque-position-if
                  #'application-input-controller--follow-up-work-p
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
       :queued-inputs queued-inputs))
    (not (null work))))

(-> application-input-controller--cycle-follow-up
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--cycle-follow-up (controller input)
  "Move CONTROLLER's recalled draft to the previous queued follow-up, wrapping."
  (let ((current-work (application-input-controller--input-work input))
        (selected-work nil)
        (steering-inputs nil)
        (queued-inputs nil))
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
                       #'application-input-controller--follow-up-work-p
                       work-items
                       :end current-position
                       :from-end t)
                      (deque-position-if
                       #'application-input-controller--follow-up-work-p
                       work-items
                       :start current-position
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
       :queued-inputs queued-inputs))
    (when current-work
      (application-input-controller--publish-counts controller))
    (not (null selected-work)))))

(-> application-input-controller--process-event
    (application-input-controller t)
    null)
(defun application-input-controller--process-event (controller event)
  "Apply terminal EVENT and publish any resulting work or exit request."
  (let ((ui (application-ui
             (application-input-controller-application controller)))
        (follow-up-editing-p
          (application-input-controller--follow-up-editing-p controller)))
    (if (and (eq event ':interrupt) follow-up-editing-p)
        (progn
          (terminal-ui-process-event
           ui event :queue-editing-p follow-up-editing-p)
          (application-input-controller--clear-follow-up-edit controller))
        (let ((active-interrupt-action
                (and (eq event ':interrupt)
                     (application-input-controller--active-turn-interrupt-action
                      controller))))
          (cond
            ((eq event ':stream-end)
             (application-input-controller--request-exit
              controller ':end-of-input))
            ((eq active-interrupt-action ':force)
             (application-input-controller--force-interrupt-exit controller))
            ((eq active-interrupt-action ':hint)
             nil)
            ((and (eq event ':interrupt)
                  (application-input-controller--request-active-turn-cancellation
                   controller
                   :force-exit-window-p t
                   :pause-queued-work-p t))
             nil)
            ((and (eq event ':escape)
                  (or
                   (application-input-controller--turn-cancellation-active-p
                    controller)
                   (application-input-controller--request-active-turn-cancellation
                    controller)))
             nil)
            (t
             (let ((turn-active-p
                     (application-input-controller-turn-active-p controller)))
               (multiple-value-bind (action payload)
                   (terminal-ui-process-event
                    ui
                    event
                    :queue-completion-p turn-active-p
                    :queue-editing-p follow-up-editing-p)
                 (case action
                   (:cleared
                    (application-input-controller--clear-follow-up-edit controller))
                   (:submit
                    (unless
                        (application-input-controller--defer-lisp-submission-p
                         controller payload)
                      (unless
                          (application-input-controller--handle-recalled-submission
                           controller payload)
                        (application-input-controller--handle-submission
                         controller
                         payload
                         :steer-p
                         (application-input-controller-turn-active-p
                          controller)))))
                    (:queue
                     (unless
                         (application-input-controller--defer-lisp-submission-p
                          controller payload)
                       (unless
                           (application-input-controller--handle-recalled-submission
                            controller payload :prefer-steering-p nil)
                         (application-input-controller--handle-queue-submission
                          controller payload))))
                   (:edit-queue
                    (application-input-controller--recall-follow-up controller))
                   (:cycle-queue
                    (application-input-controller--cycle-follow-up
                     controller payload))
                   (:end-of-input
                    (application-input-controller--request-exit
                     controller ':end-of-input))
                   (:interrupt
                    (application-input-controller--request-exit
                     controller ':interrupt))))))))))
  nil)

(-> application-input-controller--input-ready-p
    (application-input-controller)
    boolean)
(defun application-input-controller--input-ready-p (controller)
  "Apply pending resizes and report whether CONTROLLER's terminal has input."
  (let* ((ui (application-ui
              (application-input-controller-application controller)))
         (terminal (terminal-ui-terminal ui)))
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)
    (terminal-ui-refresh-status ui)
    (if (terminal-input-ready-p terminal)
        t
        (progn
          (with-lock-held ((application-input-controller-lock controller))
            (unless (or (application-input-controller-stopping-p controller)
                        (application-input-controller-reader-paused-p controller))
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)
               :timeout 0.02)))
          nil))))

(-> application-input-controller--reader-loop
    (application-input-controller)
    null)
(defun application-input-controller--reader-loop (controller)
  "Read events until pause, failure, or a completed interrupt escalation."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (loop
            (application-input-controller--refresh-interrupt-hint controller)
            (multiple-value-bind (stopping-p reader-paused-p)
                (with-lock-held
                    ((application-input-controller-lock controller))
                  (values
                   (application-input-controller-stopping-p controller)
                   (application-input-controller-reader-paused-p controller)))
              (cond
                (reader-paused-p
                 (return))
                (stopping-p
                 (let* ((application
                          (application-input-controller-application controller))
                        (ui (application-ui application))
                        (terminal (terminal-ui-terminal ui)))
                   (terminal-ui-refresh-status ui)
                   (if (terminal-input-ready-p terminal)
                       (case (terminal-read-event terminal)
                         (:interrupt
                          (application-input-controller--force-interrupt-exit
                           controller))
                         (:escape
                          nil)
                         ((:end-of-input :stream-end)
                          (return)))
                       (with-lock-held
                           ((application-input-controller-lock controller))
                         (unless
                             (application-input-controller-reader-paused-p
                              controller)
                           (condition-wait
                            (application-input-controller-condition-variable
                             controller)
                            (application-input-controller-lock controller)
                            :timeout 0.02))))))
                ((application-input-controller--input-ready-p controller)
                 (application-input-controller--process-event
                  controller
                  (application-read-terminal-event
                   (application-ui
                    (application-input-controller-application controller))))))))
        (serious-condition (condition)
          (application-input-controller--record-failure
           controller condition signal-backtrace)))))
  nil)

(-> application-input-controller--start-reader
    (application-input-controller)
    null)
(defun application-input-controller--start-reader (controller)
  "Start CONTROLLER's reader unless it is paused, stopping, or already live."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (or (application-input-controller-stopping-p controller)
                (application-input-controller-reader-paused-p controller)
                (let ((thread
                        (application-input-controller-reader-thread controller)))
                  (and thread (thread-alive-p thread))))
      (setf (application-input-controller-reader-thread controller)
            (make-thread
             (lambda ()
               (application-input-controller--reader-loop controller))
             :name "Autolith terminal input"))))
  nil)

(-> application-input-controller--pause-reader
    (application-input-controller)
    null)
(defun application-input-controller--pause-reader (controller)
  "Stop and join CONTROLLER's reader without ending the application."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-reader-paused-p controller) t
            thread (application-input-controller-reader-thread controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil)))))
  nil)

(-> application-input-controller-call-with-reader-paused
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-reader-paused
    (controller function)
  "Call FUNCTION while CONTROLLER has no competing terminal reader."
  (let ((outermost-p nil)
        (reader-thread-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf reader-thread-p
            (eq (current-thread)
                (application-input-controller-reader-thread controller)))
      (unless reader-thread-p
        (setf outermost-p
              (zerop (application-input-controller-pause-depth controller)))
        (incf (application-input-controller-pause-depth controller))))
    (when reader-thread-p
      (error 'configuration-error
             :message
             "Terminal-owning work cannot pause the current input reader. Submit it without EVAL-NOW so it can run after the active turn."))
    (when outermost-p
      (application-input-controller--pause-reader controller))
    (unwind-protect
         (funcall function)
      (let ((restart-p nil))
        (with-lock-held ((application-input-controller-lock controller))
          (decf (application-input-controller-pause-depth controller))
          (when (zerop (application-input-controller-pause-depth controller))
            (setf (application-input-controller-reader-paused-p controller) nil
                  restart-p
                  (not (application-input-controller-stopping-p controller)))))
        (when restart-p
          (application-input-controller--start-reader controller))))))

(-> application--command-authorization-items (string pathname) list)
(defun application--command-authorization-items (command directory)
  "Return the modal choices for COMMAND in DIRECTORY."
  (declare (ignore command))
  (list
   (list :name "pick"
         :argument nil
         :description "pick for me; the model chooses sandbox, full access, or refusal")
   (list :name "once"
         :argument nil
         :description "allow once inside the workspace sandbox")
   (list :name "always"
         :argument nil
         :description
         (format nil "always allow this exact command in ~A"
                 (application--abbreviated-directory (namestring directory))))
   (list :name "sandbox"
         :argument nil
         :description "allow sandboxed commands for this session")
   (list :name "full"
         :argument nil
         :description "let it ride with full user privileges for this session")
   (list :name "deny"
         :argument nil
         :description "do not run the command")))

(-> application--apply-classified-command-permission
    (application string keyword string)
    keyword)
(defun application--apply-classified-command-permission
    (application command decision reason)
  "Apply one classifier DECISION for COMMAND and explain REASON."
  (ecase decision
    (:sandboxed
     (application-present
      application
      (format nil "Picked sandbox for ~A: ~A."
              (text-cell-prefix (sanitize-text command :single-line-p t) 40)
              reason))
     ':sandboxed)
    (:full-access
     (application-present
      application
      (format nil "Picked full access for ~A: ~A."
              (text-cell-prefix (sanitize-text command :single-line-p t) 40)
              reason))
     ':full-access)
    (:deny
     (application-present
      application
      (format nil "Refused ~A: ~A."
              (text-cell-prefix (sanitize-text command :single-line-p t) 40)
              reason))
     ':deny)))

(-> application--model-command-permission
    (application string pathname)
    (values keyword string))
(defun application--model-command-permission (application command directory)
  "Return the cached or freshly inferred model permission for COMMAND."
  (let* ((cache (application-command-classifications application))
         (key (format nil "~A~%~A" command (namestring directory)))
         (cached (gethash key cache)))
    (if cached
        (values (car cached) (cdr cached))
        (progn
          (application-present
           application
           (format nil "Picking a permission for ~A."
                   (text-cell-prefix (sanitize-text command :single-line-p t)
                                     40)))
          (multiple-value-bind (decision reason)
              (permissions-model-classify-command
               command directory
               :provider (application-provider application)
               :configuration (application-configuration application))
            (unless (eq decision ':ask)
              (setf (gethash key cache) (cons decision reason)))
            (values decision reason))))))

(-> application--classified-command-permission
    (application string pathname)
    (values keyword string))
(defun application--classified-command-permission
    (application command directory)
  "Classify COMMAND with the conservative heuristics, then the model.

The heuristic floor still refuses catastrophic commands and fast-paths
trivial inspection without a model call; the model decides the rest."
  (multiple-value-bind (decision reason)
      (permissions-classify-command command)
    (if (eq decision ':ask)
        (application--model-command-permission application command directory)
        (values decision reason))))

(-> application--auto-command-permission
    (application string pathname)
    keyword)
(defun application--auto-command-permission (application command directory)
  "Classify COMMAND and grant, refuse, or ask only when the model defers."
  (multiple-value-bind (decision reason)
      (application--classified-command-permission application command
                                                  directory)
    (if (eq decision ':ask)
        (application--ask-command-permission application command directory)
        (application--apply-classified-command-permission
         application command decision reason))))

(-> application--ask-command-permission
    (application string pathname)
    keyword)
(defun application--ask-command-permission (application command directory)
  "Ask interactively how COMMAND may run in DIRECTORY, failing closed otherwise."
  (block nil
    (let* ((controller (application-input-controller application))
           (ui         (application-ui application)))
      (unless (and controller
                   ui
                   (terminal-interactive-p (terminal-ui-terminal ui)))
        (return ':deny))
      (let ((choice
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (terminal-ui-select
                  ui
                  :title
                  (format nil "run ~A"
                          (text-cell-prefix
                           (sanitize-text command :single-line-p t)
                           56))
                  :items (application--command-authorization-items
                          command directory)
                  :resize-callback #'application-pending-terminal-size)))))
        (cond
          ((or (string= (or choice "") "pick")
               (string= (or choice "") "auto"))
           (multiple-value-bind (decision reason)
               (application--classified-command-permission
                application command directory)
             (application--apply-classified-command-permission
              application command
              (if (eq decision ':ask) ':deny decision)
              reason)))
          ((string= (or choice "") "once")
           ':sandboxed)
          ((string= (or choice "") "always")
           (permissions-allow
            :configuration (application-configuration application)
            :state         (application-permission-state application)
            :command       command
            :directory     directory)
           ':sandboxed)
          ((string= (or choice "") "sandbox")
           (setf (application-permission-mode application) ':sandboxed)
           ':sandboxed)
          ((string= (or choice "") "full")
           (setf (application-permission-mode application) ':full-access)
           ':full-access)
          (t
           ':deny))))))

(-> application-authorize-command (application string pathname) keyword)
(defun application-authorize-command (application command directory)
  "Return the session, saved, classified, or interactively selected permission."
  (with-lock-held ((application-command-authorization-lock application))
    (case (application-permission-mode application)
      (:full-access
       ':full-access)
      (:sandboxed
       ':sandboxed)
      (:auto
       (if (permissions-allowed-p
            (application-permission-state application)
            command
            directory)
           ':sandboxed
           (application--auto-command-permission
            application command directory)))
      (:ask
       (if (permissions-allowed-p
            (application-permission-state application)
            command
            directory)
           ':sandboxed
           (application--ask-command-permission
            application command directory))))))

(-> application--tool-authorization-title (tool) string)
(defun application--tool-authorization-title (tool)
  "Return the complete identity of TOOL as one approval-picker title."
  (format nil
          "allow ~{~A~^, ~}"
          (mapcar
           (lambda (field)
             (destructuring-bind (label value) field
               (format nil "~A ~S" label value)))
           (tool-authorization-identity-fields tool))))

(-> application--tool-authorization-request-entry
    (tool json-object)
    string)
(defun application--tool-authorization-request-entry (tool arguments)
  "Render TOOL identity and complete ARGUMENTS for one approval request."
  (with-output-to-string (stream)
    (format stream "External tool approval requested.~%")
    (dolist (field (tool-authorization-identity-fields tool))
      (destructuring-bind (label value) field
        (format stream "  ~A  ~S~%" label value)))
    (format stream "  arguments  ~A" (json-encode arguments))))

(-> application--tool-authorization-items () list)
(defun application--tool-authorization-items ()
  "Return the modal choices for one fully displayed external tool request."
  (list
   (list :name "allow"
         :argument nil
         :description "allow this one call with the arguments shown above")
   (list :name "deny"
         :argument nil
         :description "do not call the external tool")))

(-> application--ask-tool-permission
    (application tool json-object)
    keyword)
(defun application--ask-tool-permission (application tool arguments)
  "Ask interactively whether external TOOL may run, failing closed otherwise."
  (block nil
    (let* ((controller (application-input-controller application))
           (ui         (application-ui application)))
      (unless (and controller
                   ui
                   (terminal-interactive-p (terminal-ui-terminal ui)))
        (return ':deny))
      (let ((choice
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (application-present
                  application
                  (application--tool-authorization-request-entry
                   tool arguments))
                 (terminal-ui-select
                  ui
                  :title (application--tool-authorization-title tool)
                  :items (application--tool-authorization-items)
                  :resize-callback #'application-pending-terminal-size)))))
        (if (string= (or choice "") "allow")
            ':allow
            ':deny)))))

(-> application-authorize-tool (application tool json-object) keyword)
(defun application-authorize-tool (application tool arguments)
  "Return the interactively selected permission for one external TOOL call."
  (with-lock-held ((application-command-authorization-lock application))
    (application--ask-tool-permission application tool arguments)))

(-> application-input-controller-schedule-later
    (application-input-controller string &key (:due-at timestamp) (:window string))
    later-entry)
(defun application-input-controller-schedule-later
    (controller input &key due-at window)
  "Persist INPUT for DUE-AT and wake CONTROLLER's deferred scheduler."
  (let ((application (application-input-controller-application controller)))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((entry
              (later-schedule
               :configuration (application-configuration application)
               :state (application-input-controller-later-state controller)
               :input input
               :directory (configuration-working-directory
                           (application-configuration application))
               :due-at due-at
               :window window)))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller))
        entry))))

(-> application-input-controller-cancel-later
    (application-input-controller string)
    boolean)
(defun application-input-controller-cancel-later (controller identifier)
  "Cancel deferred IDENTIFIER durably and wake CONTROLLER."
  (let ((application (application-input-controller-application controller)))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((cancelled-p
              (later-cancel
               (application-configuration application)
               (application-input-controller-later-state controller)
               identifier)))
        (when cancelled-p
          (sb-thread:condition-broadcast
           (application-input-controller-condition-variable controller)))
        cancelled-p))))


(-> application-input-controller--complete-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--complete-later (controller entry)
  "Remove successfully dispatched ENTRY from durable deferred state."
  (with-lock-held ((application-input-controller-lock controller))
    (later-cancel
     (application-configuration
      (application-input-controller-application controller))
     (application-input-controller-later-state controller)
     (later-entry-identifier entry)))
  nil)

(-> application-input-controller--retry-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--retry-later (controller entry)
  "Reschedule failed ENTRY from current rate data or a five-minute fallback."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (provider (application-provider application))
         (now (get-universal-time)))
    (multiple-value-bind (reset-at window)
        (later-reset-deadline (and provider (provider-rate-limits provider))
                              :now now)
      (let ((replacement
              (with-lock-held ((application-input-controller-lock controller))
                (prog1
                    (later-reschedule
                     :configuration configuration
                     :state (application-input-controller-later-state controller)
                     :entry entry
                     :due-at (if (and reset-at (> reset-at now))
                                 reset-at
                                 (+ now 300))
                     :window (if (and window reset-at (> reset-at now))
                                 window
                                 "5 minute retry"))
                  (sb-thread:condition-broadcast
                   (application-input-controller-condition-variable controller))))))
        (application-present
         application
         (format nil "Deferred input ~A was rescheduled after ~A."
                 (later-entry-identifier replacement)
                 (later-entry-window replacement))))))
  nil)

(-> application-input-controller-create
    (application
     &key (:initial-work-items list)
          (:load-pending-p boolean)
          (:pending-persistence-enabled-p boolean)
          (:start-reader-p boolean))
    application-input-controller)
(defun application-input-controller-create
    (application
     &key initial-work-items (load-pending-p t)
          (pending-persistence-enabled-p t) (start-reader-p t))
  "Create CONTROLLER for APPLICATION and optionally start its terminal reader."
  (let* ((configuration
           (and (slot-boundp application 'configuration)
                (application-configuration application)))
         (later-state
           (if (typep configuration 'configuration)
               (later-load configuration)
               (make-instance 'later-state)))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :initial-work-items
                          (make-deque
                           :initial-contents (copy-tree initial-work-items))
                          :later-state later-state
                          :pending-persistence-enabled-p
                          pending-persistence-enabled-p
                          :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (when load-pending-p
      (application-input-controller--load-pending controller))
    (application-input-controller--publish-counts controller)
    (when start-reader-p
      (application-input-controller--open-prompt-if-ready controller)
      (application-input-controller--start-reader controller))
    controller))

(-> application-input-controller--next-work
    (application-input-controller)
    (option list))
(defun application-input-controller--next-work (controller)
  "Wait for and return CONTROLLER's next work item, or NIL after exit."
  (let ((application (application-input-controller-application controller))
        (work nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-active-work-kind controller) nil
            (application-input-controller-active-work-interactive-p controller) nil)
      (loop
        (let ((entry
                (later-pop-due
                 (application-input-controller-later-state controller)
                 (get-universal-time))))
          (when entry
            (deque-push-back
             (application-input-controller-work-items controller)
             (list ':later entry))))
        (when (or (application-input-controller-failure controller)
                  (application-input-controller-stopping-p controller))
          (return))
        (cond
          ((and (not (application-localgroup-paused-p application))
                (not
                 (deque-empty-p
                  (application-input-controller-initial-work-items controller))))
           (setf work
                 (deque-pop-front
                  (application-input-controller-initial-work-items controller))
                 (application-input-controller-active-p controller) t)
           (return))
          ((and (not (application-localgroup-paused-p application))
                (every #'deque-empty-p
                       (application-input-controller--queues controller))
                (null
                 (application-input-controller-follow-up-edit-index controller)))
           (let ((mode (application-localgroup-take-ready-handoff application)))
             (when mode
               (setf work (list ':localgroup-handoff mode)
                     (application-input-controller-active-p controller) t)
               (return))))
          ((and (not (application-localgroup-paused-p application))
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
          ((and (not (application-localgroup-paused-p application))
                (not
                 (application-input-controller-queued-work-paused-p controller))
                (not
                 (deque-empty-p
                  (application-input-controller-work-items controller)))
                (not
                 (eql
                  (application-input-controller-follow-up-edit-index controller)
                  0)))
           (setf work
                 (deque-pop-front
                  (application-input-controller-work-items controller))
                 (application-input-controller-active-p controller) t
                 (application-input-controller-active-work-interactive-p
                  controller)
                 t)
           (if (eq (first work) ':message)
               (setf (application-input-controller-active-work controller) work
                     (application-input-controller-active-work-identifier controller)
                     (make-identifier))
               (setf (application-input-controller-active-work controller) nil
                     (application-input-controller-active-work-identifier controller)
                     nil))
           (when (application-input-controller-follow-up-edit-index controller)
             (decf
              (application-input-controller-follow-up-edit-index controller)))
           (when (plusp
                  (application-input-controller-steering-promotion-prefix-count
                   controller))
             (decf
              (application-input-controller-steering-promotion-prefix-count
               controller)))
           ;; The prior snapshot retains queued WORK until this atomic replacement
           ;; durably represents it as active work.
           (application-input-controller--persist-pending controller)
           (return)))
        (let* ((state (application-input-controller-later-state controller))
                (entry
                  (unless (later-state-active-entry state)
                    (priority-queue-peek (later-state-queue state))))
                (later-wait
                  (and entry
                       (max 0.01 (- (later-entry-due-at entry)
                                    (get-universal-time)))))
               (handoff-wait
                 (and (application-localgroup-handoff-pending-p application)
                      1/10))
               (wait-seconds
                 (cond ((and later-wait handoff-wait)
                        (min later-wait handoff-wait))
                       (later-wait later-wait)
                       (handoff-wait handoff-wait))))
          (if wait-seconds
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)
               :timeout wait-seconds)
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)))))
      (when work
        (setf (application-input-controller-active-work-kind controller)
              (first work)))
      (when (application-input-controller-failure controller)
        (error
         'application-input-failed
         :original-condition (application-input-controller-failure controller)
         :backtrace (application-input-controller-failure-backtrace controller))))
    (application-input-controller--publish-counts controller)
    work))

(-> application-input-controller--finish-work
    (application-input-controller)
    null)
(defun application-input-controller--finish-work (controller)
  "Finish current work and promote unconsumed steering at its ordered position."
  (let ((clear-notice-p nil)
        (pause-notice nil)
        (marker-work-p nil)
        (marker-status 0)
        (reopen-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-stopping-p controller)
        (let* ((work-items
                 (application-input-controller-work-items controller))
               (in-flight
                 (application-input-controller-steering-in-flight-items controller))
               (steering
                 (application-input-controller-steering-items controller))
               (steering-promotion-prefix-count
                 (min
                  (application-input-controller-steering-promotion-prefix-count
                   controller)
                  (deque-count work-items)))
               (promoted-count
                 (+ (deque-count in-flight) (deque-count steering)))
               (suffix
                 (deque-split-at work-items steering-promotion-prefix-count)))
          (deque-move-all
           in-flight work-items
           :key (lambda (entry)
                  (list ':message (agent-steering-input-content entry))))
          (deque-move-all
           steering work-items
           :key (lambda (input) (list ':message input)))
          (deque-move-all suffix work-items)
          (when (application-input-controller-follow-up-edit-index controller)
            (incf
             (application-input-controller-follow-up-edit-index controller)
             promoted-count))
          (setf (application-input-controller-steering-promotion-prefix-count
                 controller)
                (+ steering-promotion-prefix-count promoted-count))))
      (when (and (application-input-controller-turn-cancellation-p controller)
                 (not (eq (application-input-controller-exit-reason controller)
                          ':quit)))
        (setf (application-input-controller-prompt-marker-status controller) 1))
      (setf clear-notice-p
            (or (application-input-controller-turn-cancellation-p controller)
                (application-input-controller-interrupt-deadline controller))
            marker-work-p
            (application-input-controller-prompt-marker-work-p controller)
            marker-status
            (application-input-controller-prompt-marker-status controller)
            reopen-p
            (application-input-controller-prompt-marker-reopen-p controller)
            (application-input-controller-active-p controller) nil
            (application-input-controller-active-work-kind controller) nil
            (application-input-controller-active-work controller) nil
            (application-input-controller-active-work-identifier controller) nil
            (application-input-controller-active-work-interactive-p controller) nil
            (application-input-controller-prompt-marker-work-p controller) nil
            (application-input-controller-prompt-marker-status controller) 0
            (application-input-controller-prompt-marker-reopen-p controller) nil
            (application-input-controller-turn-cancellation-p controller) nil
            (application-input-controller-turn-cancellation-delivery-pending-p
             controller)
            nil
            (application-input-controller-interrupt-deadline controller) nil
            (application-input-controller-interrupt-hint-time controller) nil)
      (when (and
             (application-input-controller-queued-work-paused-p controller)
             (not
              (deque-empty-p
               (application-input-controller-work-items controller))))
        (setf pause-notice
              (format nil
                      "Interrupted. ~D queued item~:P held; submit input to resume."
                      (deque-count
                       (application-input-controller-work-items controller)))))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (cond (pause-notice
           (terminal-ui-set-notice
            (application-ui (application-input-controller-application controller))
            pause-notice
            :duration-seconds *application-interrupted-queue-notice-seconds*))
          (clear-notice-p
           (terminal-ui-set-notice
            (application-ui (application-input-controller-application controller))
            nil)))
    (application-input-controller--publish-counts controller)
    (let ((ui
            (application-ui
             (application-input-controller-application controller))))
      (when marker-work-p
        (terminal-ui-finish-prompt-block ui marker-status))
      (when reopen-p
        (application-input-controller--open-prompt-if-ready controller))))
  nil)

(-> application-input-controller-stop (application-input-controller) null)
(defun application-input-controller-stop (controller)
  "Retire CONTROLLER after shutdown work is complete and join its reader."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (mapc #'deque-clear
            (application-input-controller--queues controller))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-reader-paused-p controller) t
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
            nil
            (application-input-controller-interrupt-deadline controller) nil
            (application-input-controller-interrupt-hint-time controller) nil
            thread (application-input-controller-reader-thread controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil))))
    (let ((application (application-input-controller-application controller)))
      (when (eq controller (application-input-controller application))
        (setf (application-input-controller application) nil))))
  nil)

(-> application-input-controller-call-with-shutdown-escape
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-shutdown-escape
    (controller function)
  "Call potentially blocking shutdown FUNCTION while Ctrl-C remains effective.

Normal editor input is already disabled while FUNCTION runs. The controller's
reader stays alive in interrupt-only mode until FUNCTION returns or unwinds."
  (application-input-controller--prepare-shutdown controller ':shutdown)
  (unwind-protect
       (funcall function)
    (application-input-controller-stop controller)))

(-> application--run-message-input
    (application (or string user-message-input)
     &key (:steering-function (option function))
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
          user-message-persisted-function pending-operations-function
          pending-input-identifier (tools-p t)
          tool-allowlist (tool-restriction-p nil) (goal-continuations-p t)
          (fatal-agent-loop-errors-p t))
  "Run model INPUT with established expected, cancellation, and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (progn
            (application-run-message
             application
             input
             :steering-function steering-function
             :steering-persisted-function steering-persisted-function
             :user-message-persisted-function user-message-persisted-function
             :pending-operations-function pending-operations-function
             :pending-input-identifier pending-input-identifier
             :tools-p tools-p
             :tool-allowlist tool-allowlist
             :tool-restriction-p tool-restriction-p
             :goal-continuations-p goal-continuations-p)
            ':continue)
        (application-turn-cancelled (condition)
          (error condition))
        (application-input-failed (condition)
          (error condition))
        (rollback-requested (condition)
          (error condition))
        (agent-loop-error (condition)
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
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          (if (provider-rate-limit-error-p condition)
              ':rate-limited
              ':failed))
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application--run-command-input (application string) keyword)
(defun application--run-command-input (application input)
  "Run command INPUT through the local restart debugger."
  (let ((invocation (application-command-invocation-parse input)))
    (application-command--call-with-presentation
     invocation
     (lambda ()
       (multiple-value-bind (result condition)
           (application--call-with-command-debugger
            application
            (lambda ()
              (application-handle-input application input)))
         (application-user-operation-record-command-outcome
          application invocation :action result :condition condition)
         (application-operation-present-command-hint application invocation)
         result)))))

(-> application-input-controller--run-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--run-later (controller entry)
  "Dispatch due deferred ENTRY and durably complete or retry it."
  (block nil
    (let* ((application (application-input-controller-application controller))
           (input (later-entry-input entry)))
      (application-present
       application
       (format nil "Running deferred input ~A after its ~A reset.~%  ~A"
               (later-entry-identifier entry)
               (later-entry-window entry)
               (text-cell-prefix
                (sanitize-text input :single-line-p t)
                72)))
      (handler-case
          (application-set-working-directory
           application (later-entry-directory entry))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          (handler-case
              (application-input-controller--complete-later controller entry)
            (later-error (persistence-condition)
              (application-handle-expected-error application
                                                 persistence-condition)))
          (return nil)))
      (let* ((message (application--message-input input))
             (result
              (if message
                  (application--run-message-input application message)
                  (application--run-command-input application input))))
        (handler-case
            (if (member result '(:failed :rate-limited) :test #'eq)
                (application-input-controller--retry-later controller entry)
                (application-input-controller--complete-later controller entry))
          (later-error (condition)
            (application-handle-expected-error application condition)))
        (when (eq result ':quit)
          (application-input-controller--request-exit controller ':quit)))))
  nil)

(-> application-input-controller--defer-after-rate-limit
    (application-input-controller)
    null)
(defun application-input-controller--defer-after-rate-limit (controller)
  "Defer provider-dependent queued work after a 429.

Messages, commands, and unconsumed steering move to the provider reset deadline,
or a five-minute fallback when no reset is known. Local Lisp stays runnable at
the next application boundary because it does not depend on the provider."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (provider (application-provider application))
         (directory (configuration-working-directory configuration))
         (now (get-universal-time))
         (deferred-count 0))
    (multiple-value-bind (reset-at window)
        (later-reset-deadline (and provider (provider-rate-limits provider))
                              :now now)
      (let ((due-at (if (and reset-at (> reset-at now))
                        reset-at
                        (+ now 300)))
            (window-label (if (and window reset-at (> reset-at now))
                              window
                              "5 minute retry"))
            (failures nil))
        (with-lock-held ((application-input-controller-lock controller))
          (let ((queued-work
                  (application-input-controller-work-items controller))
                (pending (make-deque))
                (in-flight
                  (application-input-controller-steering-in-flight-items
                   controller))
                (steering
                  (application-input-controller-steering-items controller)))
            (deque-move-all
             in-flight pending
             :key (lambda (entry)
                    (list ':message (agent-steering-input-content entry))))
            (deque-move-all
             steering pending
             :key (lambda (input) (list ':message input)))
            (deque-move-if
             (lambda (item) (not (eq (first item) ':lisp)))
             queued-work pending)
            (loop until (deque-empty-p pending)
                  for item = (deque-pop-front pending)
                  for input =
                    (case (first item)
                      (:message (user-message-input-text (second item)))
                      (:command (second item))
                      (t nil))
                  when (non-empty-string-p input)
                    do (handler-case
                           (progn
                             (later-schedule
                              :configuration configuration
                              :state
                              (application-input-controller-later-state controller)
                              :input input
                              :directory directory
                              :due-at due-at
                              :window window-label)
                             (incf deferred-count))
                         (later-error (condition)
                           (push condition failures)
                           (deque-push-back queued-work item))))
            (when (plusp deferred-count)
              (sb-thread:condition-broadcast
               (application-input-controller-condition-variable controller)))))
        (dolist (condition (nreverse failures))
          (application-handle-expected-error application condition))
        (application-input-controller--publish-counts controller)
        (when (plusp deferred-count)
          (application-present
           application
           (format nil
                   "Deferred ~D queued follow-up~:P until the ~A reset."
                   deferred-count
                   window-label))))))
  nil)

(-> application-input-controller--interactive-prompt-work-p (list) boolean)
(defun application-input-controller--interactive-prompt-work-p (work)
  "Return true when WORK represents one foreground user interaction."
  (not (null (member (first work) '(:command :lisp :message) :test #'eq))))

(-> application-input-controller--begin-prompt-work
    (application-input-controller list)
    null)
(defun application-input-controller--begin-prompt-work (controller work)
  "Initialize prompt-marker state and begin execution for interactive WORK."
  (setf (application-input-controller-prompt-marker-work-p controller) nil
        (application-input-controller-prompt-marker-status controller) 0
        (application-input-controller-prompt-marker-reopen-p controller) t)
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
    (setf (application-input-controller-prompt-marker-status controller) 1))
  nil)

(-> application-input-controller--record-prompt-condition
    (application-input-controller serious-condition)
    null)
(defun application-input-controller--record-prompt-condition
    (controller condition)
  "Record escaping CONDITION and whether cancellation may reopen the next prompt."
  (setf (application-input-controller-prompt-marker-status controller) 1
        (application-input-controller-prompt-marker-reopen-p controller)
        (typep condition 'application-turn-cancelled))
  nil)

(-> application-input-controller--run-work
    (application-input-controller list)
    null)
(defun application-input-controller--run-work (controller work)
  "Run one submitted WORK item on the application main thread."
  (let ((application (application-input-controller-application controller))
        (pending-input-identifier
          (with-lock-held ((application-input-controller-lock controller))
            (application-input-controller-active-work-identifier controller))))
    (handler-case
        (progn
          (application-input-controller--begin-prompt-work controller work)
          (case (first work)
            (:apply-pending
             (application-input-controller--apply-pending-commands controller))
            (:message
             (let ((result
                     (application--run-message-input
                      application
                      (second work)
                      :steering-function
                      (lambda ()
                        (application-input-controller--take-steering controller))
                      :steering-persisted-function
                      (lambda (identifier)
                        (application-input-controller--acknowledge-steering
                         controller identifier))
                      :user-message-persisted-function
                      (lambda (identifier)
                        (application-input-controller--acknowledge-active-work
                         controller identifier))
                      :pending-operations-function
                      (lambda (agent)
                        (application-input-controller--apply-pending-commands
                         controller
                         :agent agent))
                      :pending-input-identifier pending-input-identifier)))
               (application-input-controller--record-prompt-result
                controller result)
               (when (eq result ':rate-limited)
                 (application-input-controller--defer-after-rate-limit
                  controller))))
            (:recovery-diagnosis
             (application--run-message-input
              application
              (second work)
              :tools-p t
              :tool-allowlist *application-recovery-diagnostic-tool-names*
              :tool-restriction-p t
              :goal-continuations-p nil
              :fatal-agent-loop-errors-p nil))
            (:lisp
             (let ((result
                     (application-input-controller-call-with-reader-paused
                      controller
                      (lambda ()
                        (application-run-lisp-input
                         application (second work))))))
               (application-input-controller--record-prompt-result
                controller result)
               (when (eq result ':quit)
                 (application-input-controller--request-exit
                  controller ':quit))))
            (:command
             (let* ((input (second work))
                    (invocation (application-command-invocation-parse input))
                    (command
                      (application-command-invocation-command invocation))
                    (result
                      (if (and command
                               (application-command-terminal-owner-p
                                command invocation))
                          (application-input-controller-call-with-reader-paused
                           controller
                           (lambda ()
                             (application--run-command-input application input)))
                          (application--run-command-input application input))))
               (application-input-controller--record-prompt-result
                controller result)
               (when (eq result ':quit)
                 (application-input-controller--request-exit
                  controller ':quit))))
            (:project-adaptation-offer
             (application-maybe-offer-project-adaptation application))
            (:localgroup-handoff
             (handler-case
                 (application-localgroup-run-handoff
                  application (second work) controller)
               (localgroup-error (condition)
                 (application-handle-expected-error application condition))))
            (:later
             (application-input-controller--run-later
              controller (second work)))))
      (serious-condition (condition)
        (application-input-controller--record-prompt-condition
         controller condition)
        (error condition))))
  nil)
