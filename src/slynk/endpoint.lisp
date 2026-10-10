(in-package #:autolith)

;;;; -- Slynk Endpoint --

;;; Slynk lets a person's sly session use this very image for their own Common
;;; Lisp development, beside the agent. Slynk comes from the person's own sly
;;; installation, named by :SLYNK-DIRECTORY, so its protocol matches the sly
;;; they run. The endpoint is process-wide: an ACP process serves several
;;; sessions from one image, and every one of them shares it.

(defclass slynk-endpoint ()
  ((directory
    :initarg :directory
    :reader slynk-endpoint-directory
    :type pathname
    :documentation "The slynk/ directory whose slynk.asd was loaded.")
   (port-file
    :initarg :port-file
    :initform nil
    :reader slynk-endpoint-port-file
    :type (option pathname)
    :documentation "The file receiving the port or the failure for an editor.")
   (port
    :initarg :port
    :initform nil
    :accessor slynk-endpoint-port
    :type (option (integer 0 65535))
    :documentation "The loopback port once serving; the requested port before.")
   (state
    :initarg :state
    :initform ':starting
    :accessor slynk-endpoint-state
    :type (member :starting :running :failed :stopped)
    :documentation "The endpoint lifecycle state.")
   (message
    :initform nil
    :accessor slynk-endpoint-message
    :type (option string)
    :documentation "Why the endpoint failed, when it did."))
  (:documentation "The process-wide Slynk server serving this image on loopback."))

(defvar *slynk-endpoint* nil
  "The process's Slynk endpoint, or NIL when none was started.")

(defvar *slynk-lock* (make-lock "Autolith Slynk")
  "Serializes starting, stopping and inspecting *SLYNK-ENDPOINT*.")

(defparameter *slynk-interface* "127.0.0.1"
  "The only interface Slynk listens on.")

(defparameter *slynk-quiesce-seconds* 5
  "How long stopping waits for sly connections to close.")


;;;; -- Starting and Stopping --

(-> slynk-start (configuration &key (:port (integer 0 65535)) (:background-p boolean))
    (option slynk-endpoint))
(defun slynk-start (configuration &key (port 0) background-p)
  "Start Slynk from CONFIGURATION's :SLYNK-DIRECTORY and return the endpoint.

Return NIL when no directory is configured. A running or starting endpoint is
returned unchanged. PORT 0 asks for a free port. With BACKGROUND-P, loading and
binding happen in a new thread and the endpoint is returned while :STARTING;
otherwise the call returns once the endpoint is :RUNNING or :FAILED."
  (let ((directory (config :slynk-directory configuration))
        (endpoint nil))
    (block nil
      (unless directory
        (return nil))
      (with-lock-held (*slynk-lock*)
        (when (and *slynk-endpoint*
                   (member (slynk-endpoint-state *slynk-endpoint*) '(:starting :running)))
          (return *slynk-endpoint*))
        (setf endpoint (make-instance 'slynk-endpoint
                                      :directory directory
                                      :port port
                                      :port-file (config :slynk-port-file configuration))
              *slynk-endpoint* endpoint))
      (if background-p
          (make-thread (lambda () (slynk--serve endpoint))
                       :name "Autolith Slynk start")
          (slynk--serve endpoint))
      endpoint)))

(-> slynk-stop () null)
(defun slynk-stop ()
  "Stop the running Slynk server and close its sly connections."
  (let ((endpoint (with-lock-held (*slynk-lock*) *slynk-endpoint*)))
    (when (and endpoint (eq (slynk-endpoint-state endpoint) ':running))
      (slynk--call "STOP-SERVER" (slynk-endpoint-port endpoint))
      (slynk--close-connections)
      (setf (slynk-endpoint-state endpoint) ':stopped)))
  nil)

(-> slynk-status () list)
(defun slynk-status ()
  "Return a plist describing the endpoint: :STATE, :PORT, :DIRECTORY and :MESSAGE."
  (let ((endpoint (with-lock-held (*slynk-lock*) *slynk-endpoint*)))
    (if endpoint
        (list :state (slynk-endpoint-state endpoint)
              :port (slynk-endpoint-port endpoint)
              :directory (slynk-endpoint-directory endpoint)
              :message (slynk-endpoint-message endpoint))
        (list :state ':stopped :port nil :directory nil :message nil))))

(-> slynk-call-quiesced (function) t)
(defun slynk-call-quiesced (function)
  "Call FUNCTION with Slynk stopped, then serve again on the same port.

Checkpoints call this so no sly evaluation runs while the image is saved. sly
loses its connection and reconnects to the unchanged port."
  (let ((endpoint (with-lock-held (*slynk-lock*) *slynk-endpoint*)))
    (if (and endpoint (eq (slynk-endpoint-state endpoint) ':running))
        (progn
          (slynk-stop)
          (unwind-protect
               (funcall function)
            (setf (slynk-endpoint-state endpoint) ':starting)
            (slynk--serve endpoint)))
        (funcall function))))

(-> call-with-slynk-endpoint (configuration function) t)
(defun call-with-slynk-endpoint (configuration function)
  "Call FUNCTION while CONFIGURATION's Slynk starts in the background, stopping it after."
  (slynk-start configuration :background-p t)
  (unwind-protect
       (funcall function)
    (ignore-errors (slynk-stop))))

(defmacro with-slynk-endpoint ((configuration) &body body)
  "Evaluate BODY while CONFIGURATION's Slynk serves, when one is configured.

CONFIGURATION is evaluated once before BODY. Slynk starts in the background, so
BODY never waits for it to compile, and stops when BODY exits by any means."
  `(call-with-slynk-endpoint ,configuration (lambda () ,@body)))


;;;; -- Request Context --

(define-context-contributor slynk-shared-image (request)
  "Tell the agent, while Slynk serves, that a person develops in this same image."
  (declare (ignore request))
  (destructuring-bind (&key state port &allow-other-keys) (slynk-status)
    (when (eq state ':running)
      (make-context-contribution
       :identifier "slynk-shared-image"
       :instruction "A person is connected to this very image through sly over Slynk. Their packages, definitions and state live in the active image, beside yours: inspect them with lisp.describe, lisp.source and lisp.apropos using target \"self\", or with self.eval, never in a worker. Checkpoints and restarts drop their sly connection, so avoid them unless asked."
       :evidence (format nil "Slynk listens on ~A:~D." *slynk-interface* port)
       :priority 40))))


;;;; -- Serving --

(-> slynk--serve (slynk-endpoint) null)
(defun slynk--serve (endpoint)
  "Load Slynk, bind it on loopback and publish the outcome to ENDPOINT's port file."
  (handler-case
      (progn
        (slynk--load (slynk-endpoint-directory endpoint))
        (let ((port (slynk--create-server (or (slynk-endpoint-port endpoint) 0))))
          (setf (slynk-endpoint-port endpoint) port
                (slynk-endpoint-state endpoint) ':running)
          (slynk--publish endpoint (format nil "~D~%" port))))
    (error (condition)
      (setf (slynk-endpoint-message endpoint) (princ-to-string condition)
            (slynk-endpoint-state endpoint) ':failed)
      (ignore-errors
        (slynk--publish endpoint
                        (format nil "error: ~A~%"
                                (substitute #\Space #\Newline (princ-to-string condition)))))))
  nil)

(-> slynk--load (pathname) null)
(defun slynk--load (directory)
  "Load the slynk system defined by DIRECTORY's slynk.asd, keeping compiler output away.

The exact file is loaded, so no user directory joins the ASDF search path. Slynk's
debugger and stream hooks stay off: this image's own errors and output must not
reach a sly connection."
  (let ((asd (merge-pathnames "slynk.asd" directory))
        (output (make-string-output-stream)))
    (unless (probe-file asd)
      (error 'slynk-error
             :message (format nil "No slynk.asd in ~A." (uiop:native-namestring directory))
             :operation ':load
             :reason ':missing-system))
    (let ((*standard-output* output)
          (*error-output* output)
          (*trace-output* output))
      (asdf:load-asd asd)
      (asdf:load-system "slynk"))
    (slynk--set "*GLOBAL-DEBUGGER*" nil)
    (slynk--set "*GLOBALLY-REDIRECT-IO*" nil)
    (slynk--set "*LOG-OUTPUT*" (make-broadcast-stream)))
  nil)

(-> slynk--create-server ((integer 0 65535)) (integer 0 65535))
(defun slynk--create-server (port)
  "Bind a multi-connection Slynk server on loopback PORT and return the bound port."
  (handler-case
      (let ((*standard-output* (make-broadcast-stream)))
        (slynk--call "CREATE-SERVER" :port port :dont-close t :interface *slynk-interface*))
    (error (condition)
      (error 'slynk-error
             :message (format nil "Slynk could not listen on ~A:~D: ~A"
                              *slynk-interface* port condition)
             :operation ':listen
             :reason ':bind))))

(-> slynk--close-connections () null)
(defun slynk--close-connections ()
  "Close every sly connection and wait until Slynk has dropped them."
  (let ((connections (slynk--symbol "*CONNECTIONS*")))
    (dolist (connection (symbol-value connections))
      (slynk--call "CLOSE-CONNECTION" connection nil nil))
    (loop with deadline = (+ (get-internal-real-time)
                             (* *slynk-quiesce-seconds* internal-time-units-per-second))
          while (and (symbol-value connections)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.05)))
  nil)

(-> slynk--publish (slynk-endpoint string) null)
(defun slynk--publish (endpoint text)
  "Atomically write TEXT to ENDPOINT's port file, when it has one."
  (let ((pathname (slynk-endpoint-port-file endpoint)))
    (when pathname
      (ensure-directories-exist pathname)
      (publish-file pathname text)))
  nil)

(-> slynk--symbol (string) symbol)
(defun slynk--symbol (name)
  "Return Slynk's symbol NAME, which exists only once Slynk is loaded."
  (or (find-symbol name '#:slynk)
      (error 'slynk-error
             :message (format nil "Slynk does not define ~A." name)
             :operation ':load
             :reason ':incompatible)))

(-> slynk--call (string &rest t) t)
(defun slynk--call (name &rest arguments)
  "Apply Slynk's function NAME to ARGUMENTS."
  (apply (symbol-function (slynk--symbol name)) arguments))

(-> slynk--set (string t) null)
(defun slynk--set (name value)
  "Set Slynk's global variable NAME to VALUE."
  (setf (symbol-value (slynk--symbol name)) value)
  nil)


;;;; -- Conditions --

(define-condition slynk-error (configuration-error)
  ((operation
    :initarg :operation
    :reader slynk-error-operation
    :type keyword
    :documentation "The Slynk operation that failed: :LOAD or :LISTEN.")
   (reason
    :initarg :reason
    :reader slynk-error-reason
    :type keyword
    :documentation "Why: :MISSING-SYSTEM, :INCOMPATIBLE or :BIND."))
  (:documentation "Slynk could not be loaded or could not listen."))
