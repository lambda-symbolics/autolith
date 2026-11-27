(in-package #:autolith)

;;;; -- Input Dispatch --

;;; Keys are read where the terminal is and the work runs where the agent
;;; is. A terminal UI edits its own composer; what an edit asks of the
;;; session, such as submitting the draft or cancelling the turn, goes to an
;;; input port. A direct session's port is its input controller. A terminal
;;; client's port sends the action to its channel.

(defgeneric input-port-turn-active-p (port)
  (:documentation "Return true while the session behind PORT runs a work item."))

(defgeneric input-port-follow-up-editing-p (port)
  (:documentation "Return true while a queued follow-up of PORT's session sits in the composer for editing."))

(defgeneric input-port-apply (port action &optional payload)
  (:documentation "Have the session behind PORT act on input ACTION and return true when it took it.

ACTION is one of
  :SUBMIT and :QUEUE, with the draft as PAYLOAD,
  :COMMAND, with command text as PAYLOAD, and :DETACH,
  :ESCAPE, which cancels a running turn and is refused when none runs,
  :EDIT-QUEUE, :CYCLE-QUEUE with the current draft, and :CLEARED,
  which move a queued follow-up between the queue and the composer,
  and :END-OF-INPUT, which ends the session."))

(-> terminal-ui-defer-lisp-submission-p (terminal-ui (or string user-message-input)) boolean)
(defun terminal-ui-defer-lisp-submission-p (ui input)
  "Keep incomplete Lisp INPUT in UI's composer, or refuse its image attachments, and return true then."
  (let* ((text    (user-message-input-text input))
         (async-p (input-async-lisp-p text))
         (source  (if async-p
                      (subseq text (input-async-lisp-prefix-length text))
                      text)))
    (when (or async-p (input-lisp-p text))
      (cond
        ((user-message-input-image-pathnames input)
         (terminal-ui-append-finalized
          ui (list ':lisp-input-refusal (make-identifier))
          (application-failure-line "local Lisp input cannot include image attachments"))
         (terminal-ui-set-input ui input)
         t)
        ((application-lisp-input-incomplete-p source)
         (terminal-ui-set-input
          ui (application-lisp-input-with-text
              input (concatenate 'string text (string #\Newline))))
         t)
        (t
         nil)))))

(-> terminal-ui-dispatch-event (terminal-ui t t) null)
(defun terminal-ui-dispatch-event (ui port event)
  "Apply terminal EVENT to UI's composer and hand what it asks of the session to PORT.

Ctrl-C and Ctrl-Z detach. Escape closes the completion menu before it cancels."
  (cond
    ((member event '(:detach :interrupt))
     (input-port-apply port ':detach))
    ((terminal-ui-boot-divert-event ui event)
     nil)
    ((channel-command-text event)
     (input-port-apply port ':command (channel-command-text event)))
    ((eq event ':stream-end)
     (input-port-apply port ':end-of-input))
    ((and (eq event ':escape) (terminal-ui-completion-menu-present-p ui))
     (terminal-ui-process-event ui event :queue-editing-p (input-port-follow-up-editing-p port)))
    ((and (eq event ':escape) (input-port-apply port ':escape))
     nil)
    (t
     (multiple-value-bind (action payload)
         (terminal-ui-process-event ui event
                                    :queue-completion-p (input-port-turn-active-p port)
                                    :queue-editing-p    (input-port-follow-up-editing-p port))
       (case action
         ((:cleared :edit-queue :end-of-input)
          (input-port-apply port action))
         ((:submit :queue)
          (unless (terminal-ui-defer-lisp-submission-p ui payload)
            (input-port-apply port action payload)))
         (:cycle-queue
          (input-port-apply port action payload))))))
  nil)
