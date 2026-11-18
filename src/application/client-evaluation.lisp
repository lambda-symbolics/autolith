(in-package #:autolith)

;;;; -- Client Evaluation --

;;; A client written in Common Lisp may say when it attaches that it
;;; evaluates forms. The channel then sends it
;;;
;;;   (:evaluation :id ID :forms SOURCES [:restart NAME] [:restart-value SOURCE])
;;;
;;; and the client runs the forms in its own image and answers with the same
;;; ID, whether they ran, and the rendered result or failure. This is how
;;; self.eval reaches the half of the interface that lives in the client.
;;; Clients in other languages never say they evaluate and are never asked.

(defclass client-evaluation ()
  ((identifier
    :initform      (make-identifier)
    :reader        client-evaluation-identifier
    :type          string
    :documentation "The identifier the client answers the evaluation by.")
   (attachment
    :initarg       :attachment
    :reader        client-evaluation-attachment
    :type          image-daemon:attachment
    :documentation "The attachment of the client asked to evaluate.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith client evaluation")
    :reader        client-evaluation-condition-variable
    :documentation "Signaled when the client answers.")
   (answer
    :initform      nil
    :accessor      client-evaluation-answer
    :type          list
    :documentation "The client's (SUCCEEDED-P TEXT) answer, or NIL while it has none."))
  (:documentation "One evaluation a channel waits for a client to finish."))

(-> client-evaluation-reply-p (t) boolean)
(defun client-evaluation-reply-p (packet)
  "Return true when PACKET is a terminal client's answer to an evaluation.

The answer names its evaluation and carries the text; fields a client of
another version adds are ignored."
  (values (record-check packet
                        :tag            ':evaluated
                        :keyword-keys-p t
                        :fields         (list (list :indicator ':evaluation :required t
                                                    :validate  #'stringp)
                                              (list :indicator ':text :required t
                                                    :validate  #'stringp)))))


;;;; -- Policy --

(defparameter *client-evaluation-seconds* 120
  "How long a channel waits for a client to evaluate the forms it was sent.")

(defparameter *client-evaluation-poll-seconds* 0.25
  "How often a waiting evaluation checks for a cancelled turn and a client that left.")


;;;; -- Asking --

(-> application-evaluating-attachment (application) (option image-daemon:attachment))
(defun application-evaluating-attachment (application)
  "Return the attachment of the client that evaluates forms for APPLICATION's channel, or NIL.

The controlling terminal comes first, then the structured client attached last."
  (let* ((ui         (application-ui application))
         (controller (and ui (localgroup-terminal-controller (terminal-ui-terminal ui)))))
    (find-if (lambda (attachment)
               (and (attachment-evaluates-p attachment)
                    (not (image-daemon:attachment-closed-p attachment))))
             (append (and controller (list controller))
                     (mapcar #'channel-client-attachment (application-clients application))))))

(-> application-evaluate-on-client
    (application list &key (:restart-name (option string))
                 (:restart-value-source (option string)))
    string)
(defun application-evaluate-on-client (application sources &key restart-name restart-value-source)
  "Evaluate SOURCES in the image of APPLICATION's evaluating client and return the rendered result.

Signal CLIENT-EVALUATION-ERROR when no attached client evaluates Lisp, when
the forms fail there, and when the client leaves, the turn is cancelled, or
*CLIENT-EVALUATION-SECONDS* pass without an answer."
  (let* ((hub        (application-client-hub application))
         (attachment (or (application-evaluating-attachment application)
                         (client-evaluation--fail
                          ':no-client
                          "No attached client evaluates Lisp. Use target server.")))
         (evaluation (make-instance 'client-evaluation :attachment attachment)))
    (with-lock-held ((client-hub-lock hub))
      (push evaluation (client-hub-evaluations hub)))
    (unwind-protect
         (progn
           (image-daemon:attachment-send
            attachment
            (list* :evaluation
                   :id    (client-evaluation-identifier evaluation)
                   :forms sources
                   (append (and restart-name (list :restart restart-name))
                           (and restart-value-source
                                (list :restart-value restart-value-source)))))
           (destructuring-bind (succeeded-p text)
               (application--await-evaluation application evaluation)
             (if succeeded-p
                 text
                 (client-evaluation--fail ':failed text))))
      (with-lock-held ((client-hub-lock hub))
        (setf (client-hub-evaluations hub) (remove evaluation (client-hub-evaluations hub)))))))

(-> application--await-evaluation (application client-evaluation) list)
(defun application--await-evaluation (application evaluation)
  "Wait for the client's answer to EVALUATION and return it, signaling when none will come."
  (let ((hub        (application-client-hub application))
        (controller (application-input-controller application))
        (attachment (client-evaluation-attachment evaluation))
        (deadline   (+ (get-internal-real-time)
                       (* *client-evaluation-seconds* internal-time-units-per-second))))
    (with-lock-held ((client-hub-lock hub))
      (loop
        (cond
          ((client-evaluation-answer evaluation)
           (return (client-evaluation-answer evaluation)))
          ((image-daemon:attachment-closed-p attachment)
           (client-evaluation--fail ':client-left "The client left before it answered."))
          ((and controller (application-input-controller-turn-cancellation-p controller))
           (client-evaluation--fail ':cancelled
                                    "The turn was cancelled while the client evaluated."))
          ((> (get-internal-real-time) deadline)
           (client-evaluation--fail
            ':timeout
            (format nil "The client did not answer within ~D seconds; its evaluation may still run."
                    *client-evaluation-seconds*)))
          (t
           (condition-wait (client-evaluation-condition-variable evaluation)
                           (client-hub-lock hub)
                           :timeout *client-evaluation-poll-seconds*)))))))

(-> client-evaluation--fail (keyword string) nil)
(defun client-evaluation--fail (code message)
  "Signal CLIENT-EVALUATION-ERROR with CODE and MESSAGE."
  (error 'client-evaluation-error :code code :message message))


;;;; -- Answering --

(-> application-answer-evaluation (application t &key (:succeeded-p t) (:text t)) boolean)
(defun application-answer-evaluation (application identifier &key succeeded-p text)
  "Give the pending evaluation IDENTIFIER its client's answer and return true when one took it.

An unknown or already answered evaluation, and an answer without text, take nothing."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (let ((evaluation (and (stringp identifier)
                             (stringp text)
                             (find identifier (client-hub-evaluations hub)
                                   :key  #'client-evaluation-identifier
                                   :test #'string=))))
        (when (and evaluation (null (client-evaluation-answer evaluation)))
          (setf (client-evaluation-answer evaluation)
                (list (not (null succeeded-p))
                      (bounded-string text :limit *self-evaluation-character-limit*)))
          (condition-notify (client-evaluation-condition-variable evaluation))
          t)))))


;;;; -- Evaluating in a Client --

(-> client-evaluate-request (list) list)
(defun client-evaluate-request (request)
  "Evaluate the forms of the (:evaluation ...) REQUEST in this image and return its answer's fields.

The fields are :EVALUATION with the request's identifier, :SUCCEEDED-P, and
:TEXT with the rendered output and values or the failure."
  (let ((arguments (rest request)))
    (multiple-value-bind (succeeded-p text)
        (handler-case
            (let ((sources (getf arguments :forms)))
              (unless (and (consp sources) (every #'stringp sources))
                (error 'client-evaluation-error
                       :code    ':failed
                       :message "An evaluation carries its forms as strings."))
              (values t (self-evaluate-sources
                         sources
                         :restart-name         (getf arguments :restart)
                         :restart-value-source (getf arguments :restart-value))))
          (error (condition)
            (values nil (princ-to-string condition))))
      (list :evaluation (getf arguments :id) :succeeded-p succeeded-p :text text))))

(defclass client-evaluator ()
  ((lock
    :initform      (make-lock "Autolith client evaluator")
    :reader        client-evaluator-lock
    :documentation "The lock guarding the finished answers.")
   (answers
    :initform      nil
    :accessor      client-evaluator-answers
    :type          list
    :documentation "The answer packets finished and not yet taken, newest first."))
  (:documentation "The evaluations a terminal client runs for its channel, each on a thread of its own."))

(-> client-evaluator-start (client-evaluator list) null)
(defun client-evaluator-start (evaluator request)
  "Evaluate REQUEST on a thread of its own and keep its answer for CLIENT-EVALUATOR-TAKE."
  (make-thread (lambda ()
                 (let ((answer (cons :evaluated (client-evaluate-request request))))
                   (with-lock-held ((client-evaluator-lock evaluator))
                     (push answer (client-evaluator-answers evaluator)))))
               :name "Autolith client evaluation")
  nil)

(-> client-evaluator-take (client-evaluator) list)
(defun client-evaluator-take (evaluator)
  "Return the answer packets finished since the last call, oldest first, and forget them."
  (with-lock-held ((client-evaluator-lock evaluator))
    (nreverse (shiftf (client-evaluator-answers evaluator) nil))))
