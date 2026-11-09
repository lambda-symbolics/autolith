(in-package #:autolith)

;;;; -- Localgroup Protocol Bridge --

(defvar *localgroup-startup-record* nil
  "The validated detached-process handoff record active during startup.")

(-> localgroup-startup-detached-p () boolean)
(defun localgroup-startup-detached-p ()
  "Return true while startup is reconstructing a detached localgroup process."
  (not (null *localgroup-startup-record*)))

(define-condition localgroup-error (image-daemon:daemon-error autolith-error)
  ()
  (:documentation
   "A localgroup failure joined to Autolith's condition hierarchy."))


;;;; -- image-daemon Host Wiring --

(setf image-daemon:*daemon-error-class* 'localgroup-error)
