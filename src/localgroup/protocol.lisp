(in-package #:autolith)

;;;; -- Localgroup Protocol Bridge --

(defvar *localgroup-startup-record* nil
  "The validated detached-process handoff record active during startup.")

(-> localgroup-startup-detached-p () boolean)
(defun localgroup-startup-detached-p ()
  "Return true while startup is reconstructing a detached localgroup process."
  (not (null *localgroup-startup-record*)))

(define-condition localgroup-error (image-daemon:daemon-error autolith-error)
  ((retry-p
    :initarg       :retry-p
    :initform      nil
    :reader        localgroup-error-retry-p
    :type          boolean
    :documentation "Whether the endpoint asked for the request again shortly, as during a checkpoint."))
  (:documentation
   "A localgroup failure joined to Autolith's condition hierarchy."))

(define-condition localgroup-client-rejected (localgroup-error)
  ()
  (:documentation "An authenticated endpoint explicitly refused a structured subscription."))

(define-condition localgroup-request-interrupted (localgroup-error)
  ((identifier
    :initarg       :identifier
    :reader        localgroup-request-interrupted-identifier
    :type          (integer 1)
    :documentation "The transmitted request whose reply was lost.")
   (command
    :initarg       :command
    :reader        localgroup-request-interrupted-command
    :type          keyword
    :documentation "The command that may have executed before disconnection.")
   (reconnect-id
    :initarg       :reconnect-id
    :initform      nil
    :reader        localgroup-request-interrupted-reconnect-id
    :type          (option string)
    :documentation "The planned checkpoint transition, or NIL for unexpected closure."))
  (:documentation "A transmitted request lost its reply; its execution outcome is unknown."))


;;;; -- image-daemon Host Wiring --

(setf image-daemon:*daemon-error-class* 'localgroup-error)
