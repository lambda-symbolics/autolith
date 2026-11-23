(in-package #:autolith)

;;;; -- World-Terminal Protocol Bridge --

(defvar *world-startup-record* nil
  "The validated detached-process handoff record active during startup.")

(define-condition world-terminal-error (image-daemon:daemon-error autolith-error)
  ((retry-p
    :initarg       :retry-p
    :initform      nil
    :reader        world-terminal-error-retry-p
    :type          boolean
    :documentation "Whether the endpoint asked for the request again shortly, as during a checkpoint."))
  (:documentation
   "A world-terminal failure joined to Autolith's condition hierarchy."))

(define-condition world-terminal-client-rejected (world-terminal-error)
  ()
  (:documentation "An authenticated endpoint explicitly refused a structured subscription."))

(define-condition world-terminal-request-interrupted (world-terminal-error)
  ((identifier
    :initarg       :identifier
    :reader        world-terminal-request-interrupted-identifier
    :type          (integer 1)
    :documentation "The transmitted request whose reply was lost.")
   (command
    :initarg       :command
    :reader        world-terminal-request-interrupted-command
    :type          keyword
    :documentation "The command that may have executed before disconnection.")
   (reconnect-id
    :initarg       :reconnect-id
    :initform      nil
    :reader        world-terminal-request-interrupted-reconnect-id
    :type          (option string)
    :documentation "The planned checkpoint transition, or NIL for unexpected closure."))
  (:documentation "A transmitted request lost its reply; its execution outcome is unknown."))


;;;; -- Messages --

(declaim (special *application-client-methods*))

(-> world-terminal-message-p (t) boolean)
(defun world-terminal-message-p (packet)
  "Return true for a keyword message with property fields or native client arguments.

Registered client requests carry a fixed :id prefix followed by Lisp arguments.

Each side reads only the fields it knows, so a peer of another version may add some."
  (and (consp packet)
       (keywordp (first packet))
       (if (assoc (first packet) *application-client-methods*)
           (and (eq (second packet) ':id)
                (typep (third packet) '(integer 0 *))
                (proper-list-p (nthcdr 3 packet)))
           (values (record-check (rest packet)
                                 :properties-p         t
                                 :keyword-keys-p       t
                                 :allow-duplicate-keys t)))))


;;;; -- Client Capabilities --

(defgeneric attachment-evaluates-p (attachment)
  (:documentation "Return true when ATTACHMENT's client evaluates the Common Lisp forms it is sent."))

(defmethod attachment-evaluates-p ((attachment image-daemon:attachment))
  "Answer that a client evaluates nothing unless it said so when it attached."
  (declare (ignore attachment))
  nil)

(defgeneric attachment-cancels-evaluation-p (attachment)
  (:documentation "Return true when ATTACHMENT explicitly supports best-effort evaluation cancellation."))

(defmethod attachment-cancels-evaluation-p ((attachment image-daemon:attachment))
  "Preserve old peers by sending cancellation only to attachments that advertise it."
  (declare (ignore attachment))
  nil)


;;;; -- image-daemon Host Wiring --

(setf image-daemon:*daemon-error-class* 'world-terminal-error)

(-> world-terminal--read-response (stream keyword) list)
(defun world-terminal--read-response (stream operation)
  "Read one required channel response without a time limit."
  (or (daemon-read-packet stream)
      (error 'world-terminal-error
             :message   "The channel endpoint closed without a response."
             :operation operation)))

(-> world-terminal-call (t t keyword &key (:arguments t)) list)
(defun world-terminal-call (port token operation &key arguments)
  "Send one authenticated channel OPERATION with ARGUMENTS and return its response."
  (multiple-value-bind (socket stream) (daemon-connect port)
    (declare (ignore socket))
    (unwind-protect
         (progn
           (daemon-write-packet
            stream
            (list :world-terminal-request
                  :version *daemon-protocol-version*
                  :token token :operation operation :arguments arguments))
           (world-terminal--read-response stream operation))
      ; mallet:suppress no-ignore-errors -- closing the connection must not mask its response
      (ignore-errors (close stream)))))

(-> world-terminal-runtime-notify (image-daemon:daemon-runtime) boolean)
(defun world-terminal-runtime-notify (runtime)
  "Wake RUNTIME's existing listener after a host deadline changes."
  (and (image-daemon:daemon-runtime-live-p runtime)
       (or (eq (image-daemon:daemon-runtime-server-thread runtime) (current-thread))
           (equal (world-terminal-call
                   (image-daemon:daemon-runtime-port runtime)
                   (image-daemon:daemon-runtime-token runtime) ':runtime-notify)
                  '(:runtime-notified)))))
