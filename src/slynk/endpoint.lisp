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
    :type (member :starting :running :stopping :failed :stopped)
    :documentation "The endpoint lifecycle state.")
   (wanted-p
    :initform t
    :accessor slynk-endpoint-wanted-p
    :type boolean
    :documentation "Whether serving should resume after a checkpoint.")
   (initialized-p
    :initform nil
    :accessor slynk-endpoint-initialized-p
    :type boolean
    :documentation "Whether the Slynk shutdown protocol was successfully initialized.")
   (bound-p
    :initform nil
    :accessor slynk-endpoint-bound-p
    :type boolean
    :documentation "Whether a listening socket still needs to be stopped.")
   (closing-server
    :initform nil
    :accessor slynk-endpoint-closing-server
    :type list
    :documentation "The sentinel listener record retained until its thread has exited.")
   (closing-connections
    :initform nil
    :accessor slynk-endpoint-closing-connections
    :type list
    :documentation "Connections retained until their evaluation threads have exited.")
   (closing-threads
    :initform nil
    :accessor slynk-endpoint-closing-threads
    :type list
    :documentation "Server and connection threads retained while shutdown drains.")
   (message
    :initform nil
    :accessor slynk-endpoint-message
    :type (option string)
    :documentation "Why the endpoint failed, when it did."))
  (:documentation "The process-wide Slynk server serving this image on loopback."))

(defvar *slynk-endpoint* nil
  "The process's Slynk endpoint, or NIL when none was started.")

(defvar *slynk-lock* (make-recursive-lock "Autolith Slynk")
  "Serializes the entire endpoint lifecycle, including checkpoint callbacks.")

(defvar *slynk-quiesced-p* nil
  "Bound while the lifecycle lock protects a checkpoint; reentrant starts defer.")

(defvar *slynk-observation-lock* (make-lock "Autolith Slynk closed threads")
  "Protects close-hook observations without blocking Slynk's sentinel.")

(defvar *slynk-closed-connections* nil
  "Disconnected connections whose workers may still be evaluating.")

(defvar *slynk-closed-threads* nil
  "Exact channel and sentinel thread identities retained by the close hooks.")

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
  (let ((directory (config :slynk-directory configuration)))
    (with-recursive-lock-held (*slynk-lock*)
      (block nil
        (unless directory
          (return nil))
        (when (and *slynk-endpoint*
                   (slynk-endpoint-wanted-p *slynk-endpoint*)
                   (or *slynk-quiesced-p*
                       (member (slynk-endpoint-state *slynk-endpoint*) '(:starting :running))))
          (return *slynk-endpoint*))
        (when *slynk-endpoint*
          (slynk--stop-endpoint *slynk-endpoint*))
        (let ((endpoint (make-instance 'slynk-endpoint
                                       :directory directory
                                       :port port
                                       :port-file (config :slynk-port-file configuration))))
          (setf *slynk-endpoint* endpoint)
          (unless *slynk-quiesced-p*
            (if background-p
                (make-thread (lambda () (slynk--serve endpoint))
                             :name "Autolith Slynk start")
                (slynk--serve endpoint)))
          endpoint)))))

(-> slynk-stop () null)
(defun slynk-stop ()
  "Cancel pending startup, stop the listener and wait for sly evaluations to exit."
  (with-recursive-lock-held (*slynk-lock*)
    (when *slynk-endpoint*
      (setf (slynk-endpoint-wanted-p *slynk-endpoint*) nil)
      (slynk--stop-endpoint *slynk-endpoint*)))
  nil)

(-> slynk-status () list)
(defun slynk-status ()
  "Return a plist describing the endpoint: :STATE, :PORT, :DIRECTORY and :MESSAGE."
  (with-recursive-lock-held (*slynk-lock*)
    (let ((endpoint *slynk-endpoint*))
      (if endpoint
          (list :state (slynk-endpoint-state endpoint)
                :port (slynk-endpoint-port endpoint)
                :directory (slynk-endpoint-directory endpoint)
                :message (slynk-endpoint-message endpoint))
          (list :state ':stopped :port nil :directory nil :message nil)))))

(-> slynk-call-quiesced (function) t)
(defun slynk-call-quiesced (function)
  "Call FUNCTION with Slynk stopped, then serve again on the same port.

Checkpoints call this so no sly evaluation runs while the image is saved. sly
loses its connection and reconnects to the unchanged port."
  (with-recursive-lock-held (*slynk-lock*)
    (if *slynk-quiesced-p*
        (funcall function)
        (let ((*slynk-quiesced-p* t))
          ;; A queued startup cannot load or bind until this lock is released.
          ;; A startup already holding it finishes before we enter here.
          (when *slynk-endpoint*
            (slynk--stop-endpoint *slynk-endpoint*))
          (unwind-protect
               (funcall function)
            (when (and *slynk-endpoint*
                       (slynk-endpoint-wanted-p *slynk-endpoint*))
              (setf (slynk-endpoint-state *slynk-endpoint*) ':starting)
              (let ((*slynk-quiesced-p* nil))
                (slynk--serve *slynk-endpoint*))))))))

(-> call-with-slynk-endpoint (configuration function) t)
(defun call-with-slynk-endpoint (configuration function)
  "Call FUNCTION while CONFIGURATION's Slynk starts in the background, stopping it after."
  (slynk-start configuration :background-p t)
  (unwind-protect
       (funcall function)
    (slynk-stop)))

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
  "Load and bind ENDPOINT under the lifecycle lock unless its startup was cancelled."
  (with-recursive-lock-held (*slynk-lock*)
    (when (and (eq endpoint *slynk-endpoint*)
               (slynk-endpoint-wanted-p endpoint)
               (eq (slynk-endpoint-state endpoint) ':starting)
               (not *slynk-quiesced-p*))
      (handler-case
          (progn
            (slynk--load (slynk-endpoint-directory endpoint))
            (setf (slynk-endpoint-initialized-p endpoint) t)
            ;; Loading can run user hooks that reentrantly stop or replace us.
            (when (and (eq endpoint *slynk-endpoint*)
                       (slynk-endpoint-wanted-p endpoint)
                       (eq (slynk-endpoint-state endpoint) ':starting))
              (let ((port (slynk--create-server (or (slynk-endpoint-port endpoint) 0))))
                (setf (slynk-endpoint-port endpoint) port
                      (slynk-endpoint-bound-p endpoint) t)
                ;; Slynk registers listeners asynchronously with its sentinel.
                (slynk--wait (lambda () (slynk--server port)) "listener registration")
                (setf (slynk-endpoint-closing-server endpoint) (slynk--server port)
                      (slynk-endpoint-state endpoint) ':running
                      (slynk-endpoint-message endpoint) nil)
                (slynk--publish endpoint (format nil "~D~%" port)))))
        (error (condition)
          ;; Publication can fail after binding. Drain that listener too.
          (let ((cleanup-error nil))
            (handler-case (slynk--stop-endpoint endpoint)
              (error (failure) (setf cleanup-error failure)))
            (setf (slynk-endpoint-message endpoint)
                  (if cleanup-error
                      (format nil "~A; shutdown: ~A" condition cleanup-error)
                      (princ-to-string condition))
                  (slynk-endpoint-state endpoint) ':failed
                  (slynk-endpoint-wanted-p endpoint) nil)
            (ignore-errors
              (slynk--publish endpoint
                              (format nil "error: ~A~%"
                                      (substitute #\Space #\Newline
                                                  (slynk-endpoint-message endpoint))))))))))
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
    (slynk--set "*LOG-OUTPUT*" (make-broadcast-stream))
    (slynk--install-observers))
  nil)

(-> slynk--remember-thread (t) null)
(defun slynk--remember-thread (thread)
  "Retain THREAD before Slynk forgets a channel or finishes a close hook."
  (with-lock-held (*slynk-observation-lock*)
    (setf *slynk-closed-threads* (remove-if-not #'thread-alive-p *slynk-closed-threads*))
    (when thread
      (pushnew thread *slynk-closed-threads*)))
  nil)

(-> slynk--remember-connection (t) null)
(defun slynk--remember-connection (connection)
  "Retain accepted or disconnected CONNECTION until its threads have exited."
  (with-lock-held (*slynk-observation-lock*)
    (pushnew connection *slynk-closed-connections*))
  nil)

(-> slynk--install-observers () null)
(defun slynk--install-observers ()
  "Check the Slynk shutdown protocol and observe thread identities before removal."
  (dolist (name '("CREATE-SERVER" "STOP-SERVER" "CLOSE-CONNECTION" "CONNECTION-SOCKET-IO"
                 "FIND-REGISTERED" "THREAD-FOR-EVALUATION"
                 "CONNECTION-CHANNELS" "CHANNEL-THREAD" "MULTITHREADED-CONNECTION-P"
                 "MCONN.READER-THREAD" "MCONN.CONTROL-THREAD" "MCONN.AUTO-FLUSH-THREAD"
                 "MCONN.INDENTATION-CACHE-THREAD" "MCONN.ACTIVE-THREADS"))
    (unless (fboundp (slynk--symbol name))
      (error 'slynk-error :message (format nil "Slynk does not implement ~A." name)
                        :operation ':load :reason ':incompatible)))
  (unless (and (every (lambda (name)
                        (and (fboundp (slynk--symbol name))
                             (typep (symbol-function (slynk--symbol name)) 'generic-function)))
                      '("CLOSE-CHANNEL" "THREAD-FOR-EVALUATION"))
               (find-class (slynk--symbol "CHANNEL") nil)
               (every (lambda (name) (boundp (slynk--symbol name)))
                      '("*SERVERS*" "*CONNECTIONS*" "*CONNECTION-CLOSED-HOOK*"
                        "*NEW-CONNECTION-HOOK*")))
    (error 'slynk-error :message "Slynk lacks the channel/connection shutdown protocol."
                      :operation ':load :reason ':incompatible))
  ;; CLOSE-CHANNEL removes channels before CONNECTION-CLOSED-HOOK runs. The
  ;; generic method observes the exact thread even if a person renamed it.
  (eval `(defmethod ,(slynk--symbol "CLOSE-CHANNEL") :before
             ((channel ,(slynk--symbol "CHANNEL")) &key force)
           (declare (ignore force))
           (slynk--remember-thread (slynk--call "CHANNEL-THREAD" channel))))
  ;; Retain workers before dispatch. On abort Slynk can flush user streams
  ;; after removing the worker from its active-thread list.
  (eval `(defmethod ,(slynk--symbol "THREAD-FOR-EVALUATION") :around (connection id)
           (let ((thread (call-next-method)))
             (slynk--remember-thread thread)
             thread)))
  ;; Capture acceptance before the sentinel asynchronously registers the connection.
  (pushnew 'slynk--remember-connection
           (symbol-value (slynk--symbol "*NEW-CONNECTION-HOOK*")))
  (pushnew 'slynk--remember-connection
           (symbol-value (slynk--symbol "*CONNECTION-CLOSED-HOOK*")))
  nil)

(-> slynk--create-server ((integer 0 65535)) (integer 0 65535))
(defun slynk--create-server (port)
  "Bind a multi-connection Slynk server on loopback PORT and return the bound port."
  (handler-case
      (let ((*standard-output* (make-broadcast-stream)))
        (slynk--call "CREATE-SERVER" :port port :dont-close t
                    :interface *slynk-interface* :style ':spawn))
    (error (condition)
      (error 'slynk-error
             :message (format nil "Slynk could not listen on ~A:~D: ~A"
                              *slynk-interface* port condition)
             :operation ':listen
             :reason ':bind))))

(-> slynk--wait (function string) null)
(defun slynk--wait (predicate activity)
  "Wait for PREDICATE or fail closed when ACTIVITY exceeds the shutdown deadline."
  (loop with deadline = (+ (get-internal-real-time)
                          (* *slynk-quiesce-seconds* internal-time-units-per-second))
        until (funcall predicate)
        do (when (>= (get-internal-real-time) deadline)
             (error 'slynk-error
                    :message (format nil "Timed out waiting for Slynk ~A." activity)
                    :operation ':quiesce :reason ':timeout))
           (sleep 0.01))
  nil)

(-> slynk--server ((integer 0 65535)) list)
(defun slynk--server (port)
  "Return the sentinel's listener record for PORT, or NIL."
  (find port (symbol-value (slynk--symbol "*SERVERS*")) :key #'second))

(-> slynk--connection-threads (t) list)
(defun slynk--connection-threads (connection)
  "Snapshot CONNECTION's communication, channel and active evaluation threads."
  (append
   (mapcar (lambda (channel) (slynk--call "CHANNEL-THREAD" channel))
           (slynk--call "CONNECTION-CHANNELS" connection))
   (when (slynk--call "MULTITHREADED-CONNECTION-P" connection)
     (append (loop for name in '("MCONN.READER-THREAD" "MCONN.CONTROL-THREAD"
                                "MCONN.AUTO-FLUSH-THREAD" "MCONN.INDENTATION-CACHE-THREAD")
                   collect (slynk--call name connection))
             (copy-list (slynk--call "MCONN.ACTIVE-THREADS" connection))))))

(-> slynk--stop-endpoint (slynk-endpoint) null)
(defun slynk--stop-endpoint (endpoint)
  "Drain ENDPOINT under the lifecycle lock, retaining incomplete shutdown for retry."
  (setf (slynk-endpoint-state endpoint) ':stopping)
  (when (slynk-endpoint-bound-p endpoint)
    (let ((port (slynk-endpoint-port endpoint)))
      ;; A registered listener may already have been stopped independently.
      ;; Only an unobserved creation needs to wait for sentinel registration.
      (unless (slynk-endpoint-closing-server endpoint)
        (slynk--wait (lambda () (slynk--server port)) "listener registration")
        (setf (slynk-endpoint-closing-server endpoint) (slynk--server port)))
      (when (slynk--server port)
        (slynk--call "STOP-SERVER" port))
      (setf (slynk-endpoint-bound-p endpoint) nil)))
  (when (slynk-endpoint-closing-server endpoint)
    (destructuring-bind (socket port thread) (slynk-endpoint-closing-server endpoint)
      (declare (ignore socket))
      (slynk--wait (lambda () (and (null (slynk--server port))
                                  (or (null thread) (not (thread-alive-p thread)))))
                   "listener shutdown"))
    (setf (slynk-endpoint-closing-server endpoint) nil))
  (when (slynk-endpoint-initialized-p endpoint)
    (slynk--close-connections endpoint))
  (setf (slynk-endpoint-state endpoint) ':stopped)
  nil)

(-> slynk--close-connections (slynk-endpoint) null)
(defun slynk--close-connections (endpoint)
  "Close sly connections and wait for their threads, including detached evaluations."
  (when (find-package '#:slynk)
    (let ((connections (slynk--symbol "*CONNECTIONS*")))
      ;; Keep the sentinel until all close hooks have completed. It removes a
      ;; connection from *CONNECTIONS* before invoking those hooks.
      (let ((sentinel (slynk--call "FIND-REGISTERED" (slynk--symbol "SENTINEL"))))
        (when sentinel
          (pushnew sentinel (slynk-endpoint-closing-threads endpoint))))
      (with-lock-held (*slynk-observation-lock*)
        (dolist (connection *slynk-closed-connections*)
          (pushnew connection (slynk-endpoint-closing-connections endpoint))))
      (dolist (connection (copy-list (symbol-value connections)))
        (pushnew connection (slynk-endpoint-closing-connections endpoint)))
      (dolist (connection (slynk-endpoint-closing-connections endpoint))
        (dolist (thread (slynk--connection-threads connection))
          (pushnew thread (slynk-endpoint-closing-threads endpoint)))
        ;; A previously disconnected connection may still have evaluating workers.
        ;; Close only open streams so its close hooks are not invoked twice.
        (when (open-stream-p (slynk--call "CONNECTION-SOCKET-IO" connection))
          (slynk--call "CLOSE-CONNECTION" connection nil nil)))
      (slynk--wait
       (lambda ()
         ;; A control thread can create a worker just before it is closed.
         (with-lock-held (*slynk-observation-lock*)
           (dolist (connection *slynk-closed-connections*)
             (pushnew connection (slynk-endpoint-closing-connections endpoint)))
           (dolist (thread *slynk-closed-threads*)
             (pushnew thread (slynk-endpoint-closing-threads endpoint))))
         (dolist (connection (slynk-endpoint-closing-connections endpoint))
           (dolist (thread (slynk--connection-threads connection))
             (pushnew thread (slynk-endpoint-closing-threads endpoint))))
         (and (null (symbol-value connections))
              (notany (lambda (thread) (and thread (thread-alive-p thread)))
                      (slynk-endpoint-closing-threads endpoint))))
       "connection evaluation shutdown")
      (setf (slynk-endpoint-closing-connections endpoint) nil
            (slynk-endpoint-closing-threads endpoint) nil)
      (with-lock-held (*slynk-observation-lock*)
        (setf *slynk-closed-connections* nil
              *slynk-closed-threads* nil))))
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
    :documentation "The Slynk operation that failed: :LOAD, :LISTEN or :QUIESCE.")
   (reason
    :initarg :reason
    :reader slynk-error-reason
    :type keyword
    :documentation "Why: :MISSING-SYSTEM, :INCOMPATIBLE, :BIND or :TIMEOUT."))
  (:documentation "Slynk could not be loaded, listen or finish shutting down."))
