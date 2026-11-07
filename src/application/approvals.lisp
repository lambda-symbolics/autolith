(in-package #:autolith)

;;;; -- Channel Approvals --

;;; A command or tool approval is offered to everyone who can answer it at
;;; once: the controlling terminal's picker and every structured client. The
;;; first answer wins, and everyone then hears how it was settled. Without a
;;; terminal, the turn waits for a client's answer until it is cancelled or
;;; no client is left.

(defclass channel-approval ()
  ((identifier
    :initform      (make-identifier)
    :reader        channel-approval-identifier
    :type          string
    :documentation "The identifier clients answer the approval by.")
   (choices
    :initarg       :choices
    :reader        channel-approval-choices
    :type          list
    :documentation "The names of the choices an answer may pick.")
   (answer
    :initform      nil
    :accessor      channel-approval-answer
    :type          (option string)
    :documentation "The choice a client picked, or NIL while none has."))
  (:documentation "One approval a channel waits on."))


;;;; -- Policy --

(defparameter *channel-approval-poll-seconds* 0.25
  "How often a turn waiting for a client's approval checks for its cancellation.")


;;;; -- Asking --

(-> application-approval-possible-p (application) boolean)
(defun application-approval-possible-p (application)
  "Return true when someone can answer APPLICATION's approval: a controlling terminal or a structured client that approves."
  (let ((controller (application-input-controller application))
        (ui         (application-ui application)))
    (and controller
         (or (and ui (terminal-interactive-p (terminal-ui-terminal ui)))
             (application-clients-approving-p application))
         t)))

(-> application-select-approval
    (application &key (:kind keyword) (:title string) (:items list) (:entry string))
    (option string))
(defun application-select-approval (application &key kind title items entry)
  "Offer the approval TITLE with picker ITEMS to the terminal and the clients, and return the chosen name.

KIND says what is approved, such as :command or :external-tool, and ENTRY
describes the request in plain text for clients. Return NIL when the
approval is dismissed, the turn is cancelled, or nobody is left to answer."
  (let* ((hub      (application-client-hub application))
         (ui       (application-ui application))
         (approval (make-instance 'channel-approval
                                  :choices (mapcar (lambda (item) (getf item :name)) items)))
         (choice   nil))
    (with-lock-held ((client-hub-lock hub))
      (setf (client-hub-approval hub) approval))
    (application-clients-publish
     application
     (list :approval
           :id      (channel-approval-identifier approval)
           :kind    kind
           :title   title
           :entry   entry
           :options (mapcar (lambda (item)
                              (list :value       (getf item :name)
                                    :description (getf item :description)))
                            items)))
    (unwind-protect
         (setf choice
               (if (and ui (terminal-interactive-p (terminal-ui-terminal ui)))
                   (terminal-ui-select
                    ui
                    :status-kind             ':permission
                    :title                   title
                    :items                   items
                    :resize-callback         #'application-pending-terminal-size
                    :external-event-function (lambda ()
                                               (let ((answer (channel-approval-answer approval)))
                                                 (and answer (list :approval-answer answer))))
                    :on-event                (lambda (event selector)
                                               (declare (ignore selector))
                                               (when (and (consp event)
                                                          (eq (first event) ':approval-answer))
                                                 (list :accept (second event)))))
                   (application--await-approval application approval)))
      (with-lock-held ((client-hub-lock hub))
        (application--claim-approval hub approval choice)
        (setf choice (channel-approval-answer approval)
              (client-hub-approval hub) nil))
      (application-clients-publish application
                                   (list :approval-settled
                                         :id    (channel-approval-identifier approval)
                                         :value choice)))
    choice))

(-> application--await-approval (application channel-approval) (option string))
(defun application--await-approval (application approval)
  "Wait until a client answers APPROVAL and return the answer, or NIL once the turn is cancelled or no approving client is left."
  (let ((hub        (application-client-hub application))
        (controller (application-input-controller application)))
    (with-lock-held ((client-hub-lock hub))
      (loop
        (let ((answer (channel-approval-answer approval)))
          (cond
            (answer
             (return answer))
            ((or (notany #'channel-client-approves-p (client-hub-clients hub))
                 (application-input-controller-turn-cancellation-p controller))
             (return nil))
            (t
             (condition-wait (client-hub-condition-variable hub) (client-hub-lock hub)
                             :timeout *channel-approval-poll-seconds*))))))))


;;;; -- Answering --

(-> application-answer-approval (application string string) boolean)
(defun application-answer-approval (application identifier value)
  "Answer the pending approval IDENTIFIER with the choice named VALUE, and return true when it took the answer.

An approval already settled, another approval, or a choice it does not offer
takes nothing."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (let ((approval (client-hub-approval hub)))
        (and approval
             (string= identifier (channel-approval-identifier approval))
             (application--claim-approval hub approval value))))))

(-> application--claim-approval (client-hub channel-approval (option string)) boolean)
(defun application--claim-approval (hub approval value)
  "Claim APPROVAL for VALUE while holding HUB's lock, returning whether it won."
  (when (and (eq approval (client-hub-approval hub))
             (null (channel-approval-answer approval))
             value
             (member value (channel-approval-choices approval) :test #'string=))
    (setf (channel-approval-answer approval) value)
    (condition-notify (client-hub-condition-variable hub))
    t))
