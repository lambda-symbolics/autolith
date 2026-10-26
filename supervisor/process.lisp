;;;; -- Agent Process Lifecycle --

(in-package #:autolith-supervisor)

;; This system has no Serapeum dependency. Use standard function declarations.
(declaim (ftype (function (t &key (:launcher t) (:directory t)) integer) run-agent))

(defun run-agent (command &key (launcher nil launcher-p) directory)
  "Run COMMAND after optional LAUNCHER argv, inherit descriptors, and return its exit status.
On a nonlocal exit, kill and reap the direct child. External wrappers must exec
or supervise their own descendants. DIRECTORY selects the child's working directory."
  (supervisor--validate-argv command 'command)
  (when launcher-p
    (supervisor--validate-argv launcher 'launcher))
  (let ((arguments (append launcher command))
        (process   nil))
    ;; Do not lose the process handle if an interrupt arrives during launch.
    (sb-sys:without-interrupts
      (unwind-protect
           (progn
             (setf process
                   (handler-case
                       (uiop:launch-program arguments
                                            :input        ':interactive
                                            :output       ':interactive
                                            :error-output ':interactive
                                            :directory    directory)
                     (error (cause)
                       (error 'agent-process-error :command arguments :cause cause))))
             (sb-sys:with-local-interrupts
               (values (uiop:wait-process process))))
        (when process
          (when (uiop:process-alive-p process)
            (uiop:terminate-process process :urgent t))
          (uiop:wait-process process))))))

(declaim (ftype (function (&optional list) integer) main))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Run the command-line supervisor and return an exit status without exiting Lisp."
  (handler-case
      (supervisor--call-with-interrupts
       (lambda ()
         (multiple-value-bind (command launcher) (supervisor--parse-arguments arguments)
           (if launcher
               (run-agent command :launcher launcher)
               (run-agent command)))))
    (invalid-launch-argument (condition)
      (format *error-output* "~a~%Usage: autolith-supervisor [--launcher EXECUTABLE] -- COMMAND [ARG...]~%"
              condition)
      64)
    (agent-process-error (condition)
      (format *error-output* "~a~%" condition)
      70)
    (supervisor-interrupted (condition)
      (+ 128 (supervisor-interrupted-signal condition)))))

(declaim (ftype (function (t symbol) null) supervisor--validate-argv))

(defun supervisor--validate-argv (arguments name)
  "Require a nonempty proper argv list with a nonempty executable name."
  (unless (and (consp arguments)
               (handler-case (list-length arguments) (type-error () nil))
               (every #'stringp arguments)
               (plusp (length (first arguments))))
    (error 'invalid-launch-argument :name name :value arguments))
  nil)

(declaim (ftype (function (list) (values list list)) supervisor--parse-arguments))

(defun supervisor--parse-arguments (arguments)
  "Return command and launcher argv from the explicit command-line separator."
  (let ((launcher nil))
    (when (equal (first arguments) "--launcher")
      (setf launcher (list (second arguments))
            arguments (rest (rest arguments)))
      (supervisor--validate-argv launcher 'launcher))
    (unless (equal (first arguments) "--")
      (error 'invalid-launch-argument :name 'arguments :value arguments))
    (let ((command (rest arguments)))
      (supervisor--validate-argv command 'command)
      (values command launcher))))

(define-condition invalid-launch-argument (error)
  ((name  :initarg       :name
          :reader        invalid-launch-argument-name
          :documentation "The invalid argument's name.")
   (value :initarg       :value
          :reader        invalid-launch-argument-value
          :documentation "The rejected argument value."))
  (:documentation "An invalid argv list or command-line option.")
  (:report (lambda (condition stream)
             (format stream "Invalid supervisor argument ~s: ~s."
                     (invalid-launch-argument-name condition)
                     (invalid-launch-argument-value condition)))))

(define-condition agent-process-error (error)
  ((command :initarg       :command
            :reader        agent-process-error-command
            :documentation "The attempted executable and arguments.")
   (cause   :initarg       :cause
            :reader        agent-process-error-cause
            :documentation "The underlying launch condition."))
  (:documentation "The supervisor could not start its child.")
  (:report (lambda (condition stream)
             (format stream "Cannot start ~s: ~a"
                     (first (agent-process-error-command condition))
                     (agent-process-error-cause condition)))))

(define-condition supervisor-interrupted (error)
  ((signal :initarg       :signal
           :reader        supervisor-interrupted-signal
           :documentation "The numeric interruption signal."))
  (:documentation "An interruption that requests child cleanup and supervisor exit."))
