;;;; -- Windows Supervisor Interrupts --

(in-package #:autolith-supervisor)

(declaim (ftype (function (function) t) supervisor--call-with-interrupts))

(defun supervisor--call-with-interrupts (function)
  "Translate SBCL console interruption into a supervised shutdown."
  (handler-bind ((sb-sys:interactive-interrupt
                  (lambda (condition)
                    (declare (ignore condition))
                    (error 'supervisor-interrupted :signal 2))))
    (funcall function)))

(declaim (ftype (function () list) supervisor--shell-runtime-roots))

(defun supervisor--shell-runtime-roots ()
  "Use explicit native runtime roots for Windows shell installations."
  nil)
