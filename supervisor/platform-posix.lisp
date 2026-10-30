;;;; -- POSIX Supervisor Interrupts --

(in-package #:autolith-supervisor)

(declaim (ftype (function (function) t) supervisor--call-with-interrupts))

(defun supervisor--call-with-interrupts (function)
  "Install process-level CLI termination handlers, then call FUNCTION."
  ;; MAIN owns this standalone process. SBCL does not return old signal handlers.
  (dolist (signal (list sb-unix:sigint sb-unix:sigterm sb-unix:sighup))
    (sb-sys:enable-interrupt
     signal
     (lambda (number information context)
       (declare (ignore information context))
       (error 'supervisor-interrupted :signal number))))
  (funcall function))

(declaim (ftype (function () list) supervisor--shell-runtime-roots))

(defun supervisor--shell-runtime-roots ()
  "Admit the public POSIX shell directory for inherited native command execution."
  (list "/bin/"))
