(in-package #:autolith)

;;;; -- Device Authentication Test Support --

(defvar *device-authentication-test-saved-credentials* nil
  "The credentials observed by the recording test store.")

(defclass recording-autolith-credential-source (autolith-credential-source)
  ()
  (:documentation "An Autolith credential source that records rather than writes test data."))

(defmethod credential-source-save
    ((source recording-autolith-credential-source)
     (credentials oauth-credentials))
  "Record CREDENTIALS without touching SOURCE's pathname."
  (declare (ignore source))
  (setf *device-authentication-test-saved-credentials* credentials)
  credentials)

(-> device-authentication-test--request (list string) (option list))
(defun device-authentication-test--request (requests suffix)
  "Return the first recorded request whose URL ends in SUFFIX."
  (find-if
   (lambda (request)
     (let ((url (getf request :url)))
       (uiop:string-suffix-p url suffix)))
   requests))

(-> device-authentication-test--signals
    (function keyword &key (:status (option integer)) (:code (option string)))
    null)
(defun device-authentication-test--signals
    (function stage &key status code)
  "Assert that FUNCTION signals a safe device error for STAGE."
  (let ((signaled-p nil))
    (handler-case
        (funcall function)
      (device-authentication-error (condition)
        (setf signaled-p t)
        (test-assert (eq (device-authentication-error-stage condition) stage)
                     "the device error reports the failed stage")
        (test-assert (eql (device-authentication-error-status condition) status)
                     "the device error reports only the expected status")
        (test-assert (equal (device-authentication-error-code condition) code)
                     "the device error reports only the expected OAuth code")))
    (test-assert signaled-p "the device operation signals its expected condition")
    nil))
