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
    :documentation "The client's (SUCCEEDED-P TEXT &OPTIONAL SECTIONS) answer, or NIL while it has none."))
  (:documentation "One evaluation a channel waits for a client to finish."))

(-> client-evaluation-sections-p (t) boolean)
(defun client-evaluation-sections-p (sections)
  "Check the two bounded text sections in a successful client reply."
  (and (typep sections '(cons (eql :output) (cons string (cons (eql :values) (cons string null)))))
       (<= (length (second sections)) *evaluation-section-character-limit*)
       (<= (length (fourth sections)) *evaluation-section-character-limit*)))

(-> client-evaluation-reply-p (t) boolean)
(defun client-evaluation-reply-p (packet)
  "Return true when PACKET is a terminal client's answer to an evaluation.

The answer carries text and may include separately bounded evaluation sections."
  (values (record-check packet
                        :tag            ':evaluated
                        :keyword-keys-p t
                        :fields         (list (list :indicator ':evaluation :required t
                                                    :validate  #'non-empty-string-p)
                                              (list :indicator ':succeeded-p :required t
                                                    :validate (lambda (value) (typep value 'boolean)))
                                              (list :indicator ':text :required t
                                                    :validate #'stringp)
                                              (list :indicator ':sections
                                                     :validate #'client-evaluation-sections-p)))))

(-> client-evaluation-request-p (t) boolean)
(defun client-evaluation-request-p (packet)
  "Return true for an identified evaluation envelope with well-formed sources and restart fields."
  (values (record-check packet
                        :tag            ':evaluation
                        :keyword-keys-p t
                        :fields         (list (list :indicator ':id :required t :validate #'non-empty-string-p)
                                              (list :indicator ':forms :required t
                                                    :validate (lambda (sources)
                                                                (and (consp sources) (proper-list-p sources)
                                                                     (every #'stringp sources))))
                                              (list :indicator ':restart :validate #'stringp)
                                              (list :indicator ':restart-value :validate #'stringp)))))


;;;; -- Policy --

(defparameter *client-evaluation-seconds* 120
  "How long a channel waits for a client to evaluate the forms it was sent.")

(defparameter *client-evaluation-poll-seconds* 0.25
  "How often a waiting evaluation checks for a cancelled turn and a client that left.")


;;;; -- Asking --

(-> application-evaluating-attachment (application) (option image-daemon:attachment))
(defun application-evaluating-attachment (application)
  "Return the attachment of the client that evaluates forms for APPLICATION's channel, or NIL.

The terminal client its user is at comes first, then the client attached last."
  (let ((controller (application-controlling-client application)))
    (find-if (lambda (attachment)
               (and (attachment-evaluates-p attachment)
                    (not (image-daemon:attachment-closed-p attachment))))
             (mapcar #'channel-client-attachment
                     (append (and controller (list controller))
                             (application-clients application))))))

(-> application-evaluate-on-client
    (application list &key (:restart-name (option string))
                 (:restart-value-source (option string)))
    (values string list))
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
      (when (find attachment (client-hub-evaluations hub)
                  :key #'client-evaluation-attachment :test #'eq)
        (client-evaluation--fail ':busy "The attachment already has a pending evaluation."))
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
           (destructuring-bind (succeeded-p text &optional sections)
               (application--await-evaluation application evaluation)
             (if succeeded-p
                 (values text sections)
                 (client-evaluation--fail ':failed text))))
      (let ((abandoned-p nil))
        (with-lock-held ((client-hub-lock hub))
          (setf abandoned-p (null (client-evaluation-answer evaluation))
                (client-hub-evaluations hub) (remove evaluation (client-hub-evaluations hub))))
        (when (and abandoned-p (attachment-cancels-evaluation-p attachment))
          ;; Cancellation never waits for peer cooperation or sends unknown packets to old peers.
          (handler-case
              (image-daemon:attachment-try-send
               attachment (list :cancel-evaluation :id (client-evaluation-identifier evaluation)))
            (error () nil)))))))

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

(-> application-answer-evaluation
    (application t &key (:attachment (option image-daemon:attachment)) (:succeeded-p t)
                 (:text t) (:sections list)) boolean)
(defun application-answer-evaluation
    (application identifier &key attachment (succeeded-p nil succeeded-p-p) text sections)
  "Accept IDENTIFIER's answer only from its originating ATTACHMENT with an explicit boolean status.

Unknown or answered evaluations, malformed answers and other attachments take nothing."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (let ((evaluation (and (non-empty-string-p identifier)
                             attachment succeeded-p-p (typep succeeded-p 'boolean)
                             (stringp text)
                             (find identifier (client-hub-evaluations hub)
                                   :key #'client-evaluation-identifier :test #'string=))))
        (when (and evaluation
                   (eq attachment (client-evaluation-attachment evaluation))
                   (null (client-evaluation-answer evaluation)))
          (setf (client-evaluation-answer evaluation)
                (list* succeeded-p
                       (bounded-string text :limit *self-evaluation-character-limit*)
                       (when (and succeeded-p sections) (list sections))))
          (condition-notify (client-evaluation-condition-variable evaluation))
          t)))))


;;;; -- Evaluating in a Client --

(-> client-evaluate-request (t) list)
(defun client-evaluate-request (request)
  "Evaluate the forms of the (:evaluation ...) REQUEST in this image and return its answer's fields.

Include the request identifier, status, text, and bounded sections on success."
  (unless (client-evaluation-request-p request)
    (error 'client-evaluation-error :code ':invalid-request
           :message                       "An evaluation requires an identifier, forms as strings, and valid optional restart fields."))
  (let ((arguments (rest request)))
    (multiple-value-bind (succeeded-p text sections)
        (handler-case
            (multiple-value-bind (text sections)
                (self-evaluate-sources (getf arguments :forms)
                                       :restart-name         (getf arguments :restart)
                                       :restart-value-source (getf arguments :restart-value))
              (values t text sections))
          (error (condition)
            (let ((stream (bounded-output-stream-create *self-evaluation-character-limit*)))
              (princ condition stream)
              (values nil (bounded-output-stream-text stream) nil))))
      (list* :evaluation (getf arguments :id) :succeeded-p succeeded-p
             :text (bounded-string text :limit *self-evaluation-character-limit*)
             (when sections (list :sections sections))))))

(defclass client-evaluator ()
  ((work :initform nil :accessor client-evaluator-work
         :documentation "This owner's current work token, or NIL after abandonment.")
   (closed-p :initform nil :accessor client-evaluator-closed-p :type boolean
             :documentation "Whether this attachment stopped accepting evaluation work and replies.")
   (delivery :initarg :delivery :initform nil :accessor client-evaluator-delivery
             :type          (option function)
             :documentation "The bounded transport callback a finished answer is handed to, or NIL without one."))
  (:documentation "Attachment-owned evaluation delivery, independent of an uncooperative worker."))

(defclass client-evaluator-work ()
  ((identifier :initarg :identifier :accessor client-evaluator-work-identifier :type string
               :documentation "The identifier of the latest request admitted on this token.")
   (handoff :initform nil :accessor client-evaluator-work-handoff :type (or list (eql :publishing))
            :documentation "NIL during evaluation, :PUBLISHING while answering, or the one successor request.")
   (owner :initarg :owner :accessor client-evaluator-work-owner :type (option client-evaluator)
          :documentation "The severable delivery owner; NIL means discard any late result."))
  (:documentation "A small token for the one evaluation admitted in this client image."))

(defvar *client-evaluator-lock* (make-lock "Autolith client evaluation admission")
  "Guard shared admission and owner state, never user code or transport writes.")

(defvar *client-evaluator-active* nil
  "The admitted work token, held until its worker really exits, including abandoned work.")

(-> client-evaluator-close (client-evaluator) null)
(defun client-evaluator-close (evaluator)
  "Abandon EVALUATOR without waiting for Lisp code; sever its work and delivery references."
  (with-lock-held (*client-evaluator-lock*)
    (when-let ((work (client-evaluator-work evaluator)))
      (setf (client-evaluator-work-handoff work) nil
            (client-evaluator-work-owner work) nil))
    (setf (client-evaluator-work evaluator) nil
          (client-evaluator-delivery evaluator) nil
          (client-evaluator-closed-p evaluator) t))
  nil)

(-> client-evaluator-cancel (client-evaluator t) null)
(defun client-evaluator-cancel (evaluator identifier)
  "Discard IDENTIFIER's reply and abandon its work without releasing occupied image capacity."
  (with-lock-held (*client-evaluator-lock*)
    (let ((work (client-evaluator-work evaluator)))
      (when (and (stringp identifier) work
                 (equal identifier (client-evaluator-work-identifier work)))
        (setf (client-evaluator-work-owner work) nil
              (client-evaluator-work-handoff work) nil
              (client-evaluator-work evaluator) nil))))
  nil)

(-> client-evaluator--deliver (client-evaluator list &optional (option client-evaluator-work)) null)
(defun client-evaluator--deliver (evaluator packet &optional work)
  "Hand a bounded PACKET to EVALUATOR's delivery callback outside the admission lock.

A closed owner, one without a callback, and one that WORK no longer belongs to get nothing."
  (when-let ((delivery (with-lock-held (*client-evaluator-lock*)
                         (when (and (not (client-evaluator-closed-p evaluator))
                                    (or (null work) (eq evaluator (client-evaluator-work-owner work))))
                           (when work
                             (setf (client-evaluator-work-handoff work) ':publishing))
                           (client-evaluator-delivery evaluator)))))
    (handler-case (funcall delivery packet)
      (error ()
        (client-evaluator-close evaluator))))
  nil)

(-> client-evaluator--finish (client-evaluator-work boolean) list)
(defun client-evaluator--finish (work delivered-p)
  "Claim WORK's single successor or release admission, atomically with respect to new requests."
  (with-lock-held (*client-evaluator-lock*)
    (let ((next  (shiftf (client-evaluator-work-handoff work) nil))
          (owner (client-evaluator-work-owner work)))
      (cond
        ((and delivered-p owner (consp next))
         next)
        (t
         (when owner
           (setf (client-evaluator-work owner) nil
                 (client-evaluator-work-owner work) nil))
         (setf *client-evaluator-active* nil))))))

(-> client-evaluator--run (client-evaluator-work t) null)
(defun client-evaluator--run (work request)
  "Evaluate and deliver serially; an abandoned form holds no delivery owner."
  (loop while request
        do (let ((delivered-p nil))
             (unwind-protect
                  (let* ((packet (cons :evaluated (client-evaluate-request request)))
                         (owner  (with-lock-held (*client-evaluator-lock*)
                                   (client-evaluator-work-owner work))))
                    (when owner
                      (client-evaluator--deliver owner packet work)
                      (setf delivered-p t)))
               (setf request (client-evaluator--finish work delivered-p)))))
  nil)

(-> client-evaluator--launch (client-evaluator-work t) null)
(defun client-evaluator--launch (work request)
  "Start a worker from a scope that cannot capture its attachment owner."
  (make-thread (lambda () (client-evaluator--run work request))
               :name "Autolith client evaluation")
  nil)

(-> client-evaluator-start (client-evaluator t) keyword)
(defun client-evaluator-start (evaluator request)
  "Admit one shared image worker or answer busy; a hung abandoned worker keeps its slot."
  (unless (client-evaluation-request-p request)
    (return-from client-evaluator-start ':invalid))
  (let ((work   nil)
        (status nil))
    (with-lock-held (*client-evaluator-lock*)
      (setf status
            (cond
              ((client-evaluator-closed-p evaluator)
               ':closed)
              ((and (client-evaluator-work evaluator)
                    (eq (client-evaluator-work-handoff (client-evaluator-work evaluator)) ':publishing))
               ;; The peer can see the reply before the writer returns. Let the same worker
               ;; take its next request after delivery, without starting a second writer.
               (setf (client-evaluator-work-handoff (client-evaluator-work evaluator)) request
                     (client-evaluator-work-identifier (client-evaluator-work evaluator)) (getf (rest request) :id))
               ':accepted)
              (*client-evaluator-active*
               ':busy)
              (t
               (setf work (make-instance 'client-evaluator-work
                                         :identifier (getf (rest request) :id) :owner evaluator)
                     *client-evaluator-active* work
                     (client-evaluator-work evaluator) work)
               ':accepted))))
    (cond
      (work
       (handler-case (client-evaluator--launch work request)
         (error (condition)
           (client-evaluator-cancel evaluator (client-evaluator-work-identifier work))
           (with-lock-held (*client-evaluator-lock*)
             (when (eq work *client-evaluator-active*) (setf *client-evaluator-active* nil)))
           (error condition))))
      ((eq status ':busy)
       (client-evaluator--deliver
        evaluator (list :evaluated :evaluation (getf (rest request) :id) :succeeded-p nil
                        :text "Client evaluation is busy; abandoned work retains capacity until it exits."))))
    status))
