(in-package #:autolith)

;;;; -- Live Side Status --

(-> application-side-status-command-p (t) boolean)
(defun application-side-status-command-p (command)
  "Identify the built-in display-only command, including its registered aliases."
  (and (typep command 'application-command)
       (eq (application-command-definition-name command)
           'application--builtin-side-status-command)))

(-> application-side-status-source-p (application string) boolean)
(defun application-side-status-source-p (application source)
  "Recognize a top-level status call whose argument forms are immediate data."
  (handler-case
      (let* ((*package* (find-package '#:autolith))
             (form      (self-read-form source :read-eval nil))
             (operation (and (proper-list-p form)
                             (symbolp (first form))
                             (application-operation-find application (first form)))))
        (and (typep operation 'application-command-operation)
             (application-operation-immediate-arguments-p operation (rest form))
             (application-side-status-command-p
              (application-operation-backend operation))))
    (error ()
      nil)))

(-> application-side-status--text (t) string)
(defun application-side-status--text (value)
  "Return one bounded display-safe line for live VALUE."
  (conversation-user-operation--bounded-text
   (sanitize-text (princ-to-string value) :single-line-p t) 240))

(-> application-side-status--focus (application) string)
(defun application-side-status--focus (application)
  "Read the current objective, or the latest ordinary user input, from memory."
  (or (getf (application-goal application) :objective)
      (loop for item in (reverse (conversation-input-items
                                  (application-conversation application)))
            when (and (json-object-p item)
                      (eq (item-role item) ':user))
              do (let ((text (context--message-text item)))
                   (when (non-empty-string-p text)
                     (return text))))
      "no current focus"))

(-> application-side-status--task-states (application) list)
(defun application-side-status--task-states (application)
  "Read owned live job states, falling back to presentations before agent startup."
  (let ((orchestrator (application--task-orchestrator application))
        (viewer       (and (slot-boundp application 'agent) (application-agent application)))
        (ui           (application-ui application)))
    (cond
      ((and orchestrator viewer)
       (mapcar #'job-state
               (task-orchestrator-list-visible-jobs orchestrator viewer)))
      (ui
       (with-terminal-ui-locked (ui)
         (mapcar (lambda (activity) (getf activity :state))
                 (append (terminal-ui--command-presentation-activities ui)
                         (terminal-ui-agent-activities ui)))))
      (t
       nil))))

(-> application-side-status-snapshot (application) list)
(defun application-side-status-snapshot (application)
  "Copy live status without a provider request, tool invocation, or disk read.

Elapsed time follows the current UI activity clock. Worked time includes completed
turns and the active primary turn. Live tasks are queued or running owned jobs;
active tasks are running jobs. ETA is deliberately unknown."
  (let* ((conversation (application-conversation application))
         (ui           (application-ui application))
         (states       (application-side-status--task-states application))
         (live         (count-if (lambda (state) (member state '(:queued :running))) states))
         (active       (count ':running states))
         (focus        (application-side-status--text
                        (application-side-status--focus application))))
    (if (null ui)
        (list :focus focus :elapsed 0
              :worked (conversation-working-seconds conversation)
              :live-tasks live :active-tasks active :queue 0
              :step "idle" :eta ':unknown)
        (with-terminal-ui-locked (ui)
          (let* ((now     (funcall (terminal-ui-clock-function ui)))
                 (elapsed (terminal-ui--status-times-at ui now))
                 (step    (or (terminal-ui-local-activity ui) (terminal-ui-status ui))))
            (list :focus focus :elapsed elapsed
                  :worked (if (terminal-ui-status ui)
                              (+ (or (terminal-ui-status-worked-seconds ui)
                                     (conversation-working-seconds conversation))
                                 elapsed)
                              (conversation-working-seconds conversation))
                  :live-tasks live :active-tasks active
                  :queue  (+ (length (terminal-ui-steering-input-previews ui))
                             (length (terminal-ui-queued-input-previews ui)))
                  :step   (if step (application-side-status--text step) "idle")
                  :eta    ':unknown))))))

(-> application-side-status-entry (list (option keyword)) list)
(defun application-side-status-entry (snapshot selector)
  "Render selected live SNAPSHOT fields beneath the status jump-point symbol."
  (append
   (list (terminal-span ':notice "◆ status")
         (terminal-span ':plain (string #\Newline)))
   (application--entry-fields
    (loop for (key label style value) in
          (list (list ':focus "focus" ':brand (getf snapshot :focus))
                (list ':elapsed "elapsed" ':code
                      (format nil "~A · worked ~A"
                              (terminal-ui--duration-text (getf snapshot :elapsed))
                              (terminal-ui--duration-text (getf snapshot :worked))))
                (list ':tasks "tasks" ':notice
                      (format nil "~D live · ~D active"
                              (getf snapshot :live-tasks)
                              (getf snapshot :active-tasks)))
                (list ':queue "queue" ':code (getf snapshot :queue))
                (list ':step "step" ':plain (getf snapshot :step))
                (list ':eta "eta" ':hint "unknown"))
          when (or (null selector) (eq key selector))
            collect (list label style value)))))


;;;; -- Display-Only History --

(-> application-side-status--validate-record (conversation list) list)
(defun application-side-status--validate-record (conversation properties)
  "Validate bounded semantic output before replay without projecting model input."
  (unless (and (record-check
                properties :properties-p t :keyword-keys-p t :maximum-length 8
                :fields '((:indicator :seq :required t)
                          (:indicator :time :required t)
                          (:indicator :source :required t)
                          (:indicator :spans :required t)))
               (typep (getf properties :seq) '(integer 1))
               (typep (getf properties :time) 'timestamp)
               (typep (getf properties :source) 'string)
               (<= (length (getf properties :source)) 4096)
               (proper-list-p (getf properties :spans))
               (<= (length (getf properties :spans)) 128)
               (every #'terminal-span-p (getf properties :spans))
               (<= (loop for span in (getf properties :spans)
                         sum (length (terminal-span-text span)))
                   16384))
    (conversation--record-error conversation properties
                                "A persisted side status has invalid display data."))
  properties)

(defmethod conversation--project-record
    ((kind (eql :side-status)) (conversation conversation) properties)
  "Validate display-only history without adding input or operation context."
  (declare (ignore kind))
  (application-side-status--validate-record conversation properties)
  nil)

(-> application-side-status-present (application string list) null)
(defun application-side-status-present (application source spans)
  "Persist SPANS once and present them with the same identity used after resume."
  (let* ((conversation (application-conversation application))
         (properties   (list :source (conversation-user-operation--bounded-text source 4096)
                             :spans  spans)))
    (application-side-status--validate-record
     conversation (list* :seq  (conversation-next-sequence conversation)
                         :time (get-universal-time) properties))
    (let ((record (conversation-append-record
                   conversation (list* :side-status properties))))
      (when (application-ui application)
        (application--present-conversation-record application record))))
  nil)

(-> application-side-status--selector (t) (option keyword))
(defun application-side-status--selector (value)
  "Normalize a native selector or its slash spelling without interning input."
  (let* ((options  '(:eta :focus :elapsed :tasks :queue :step))
         (selector (if (stringp value)
                       (find value options :key #'symbol-name :test #'string-equal)
                       (and (member value options) value))))
    (when (and value (null selector))
      (error 'configuration-error
             :message "Choose ETA, focus, elapsed, tasks, queue, or step."))
    selector))

(define-application-command application--builtin-side-status-command
    (:name              "/btw"
     :aliases           ("/side")
     :static-options    (:eta :focus :elapsed :tasks :queue :step)
     :description       "Inspect live status without interrupting the current turn."
     :tip               "Use `(btw)` or `(side :focus)` for live, display-only status."
     :busy-behavior     :execute
     :terminal-behavior :shared
     :callable          t)
    (application &optional selector)
  "Show live status and retain its display outside model context."
  (setf selector (application-side-status--selector selector))
  (application-side-status-present
   application
   (if *application-command-presentation-invocation*
       (application-command-invocation-input
        *application-command-presentation-invocation*)
       "(btw)")
   (application-side-status-entry
    (application-side-status-snapshot application) selector))
  ':continue)
