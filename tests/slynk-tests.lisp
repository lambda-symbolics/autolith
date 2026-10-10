(in-package #:autolith)

;;;; -- Slynk Test Support --

;;; A stub Slynk exposes the handful of symbols the endpoint uses and records
;;; how it was called, so the lifecycle is checked without a real sly. One
;;; case loads the real Slynk when this host has a sly checkout.

(defparameter *slynk-test-stub-system*
  "(asdf:defsystem \"slynk\" :components ((:file \"slynk\")))
"
  "The stub slynk.asd text.")

(defparameter *slynk-test-stub-source*
  "(defpackage #:slynk (:use #:cl))
(in-package #:slynk)
(defvar *global-debugger* t)
(defvar *globally-redirect-io* :started-from-emacs)
(defvar *log-output* nil)
(defvar *connections* (list :first :second))
(defvar *calls* nil)
(defvar *fail-p* nil)
(defun create-server (&key port dont-close interface)
  (push (list :create port dont-close interface) *calls*)
  (when *fail-p* (error \"address already in use\"))
  (if (zerop port) 4711 port))
(defun stop-server (port)
  (push (list :stop port) *calls*))
(defun close-connection (connection condition backtrace)
  (declare (ignore condition backtrace))
  (setf *connections* (remove connection *connections*)))
"
  "The stub slynk.lisp text.")

(-> slynk-test-write-stub (pathname) pathname)
(defun slynk-test-write-stub (root)
  "Write the stub Slynk under ROOT and return its directory."
  (let ((directory (merge-pathnames "sly/slynk/" root)))
    (ensure-directories-exist directory)
    (publish-file (merge-pathnames "slynk.asd" directory) *slynk-test-stub-system*)
    (publish-file (merge-pathnames "slynk.lisp" directory) *slynk-test-stub-source*)
    directory))

(-> slynk-test-configuration (pathname (option pathname)) configuration)
(defun slynk-test-configuration (root directory)
  "Return a configuration under ROOT serving Slynk from DIRECTORY."
  (configuration-create
   :source-root       (asdf:system-source-directory :autolith)
   :working-directory (asdf:system-source-directory :autolith)
   :slynk-directory   directory
   :slynk-port-file   (merge-pathnames "editor/slynk.port" root)))

(-> slynk-test-stub-value (string) t)
(defun slynk-test-stub-value (name)
  "Return the stub Slynk's variable NAME."
  (symbol-value (find-symbol name '#:slynk)))

(-> slynk-test-port-file (configuration) (option string))
(defun slynk-test-port-file (configuration)
  "Return CONFIGURATION's port file text, or NIL when it was not written."
  (let ((pathname (config :slynk-port-file configuration)))
    (and (probe-file pathname) (uiop:read-file-string pathname))))

(-> test-slynk-directory () (option pathname))
(defun test-slynk-directory ()
  "Return a real Slynk directory on this host, or NIL.

AUTOLITH_TEST_SLYNK_DIRECTORY wins; otherwise the newest Quicklisp sly release."
  (let ((named (uiop:getenv "AUTOLITH_TEST_SLYNK_DIRECTORY")))
    (if (and named (plusp (length named)))
        (let ((directory (uiop:ensure-directory-pathname named)))
          (and (probe-file (merge-pathnames "slynk.asd" directory)) directory))
        (let ((candidates (uiop:directory*
                           (merge-pathnames "quicklisp/dists/quicklisp/software/sly-*/slynk/slynk.asd"
                                            (user-homedir-pathname)))))
          (and candidates
               (uiop:pathname-directory-pathname
                (first (sort (copy-list candidates) #'string> :key #'namestring))))))))


;;;; -- Lifecycle --

(defun test-slynk-start-uses-configured-directory ()
  "Slynk loads from the exact asd, turns its global hooks off and publishes the port."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (slynk-test-write-stub root))
           (configuration (slynk-test-configuration root directory))
           (registry (copy-list asdf:*central-registry*))
           (endpoint (slynk-start configuration)))
      (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                   "The endpoint serves after a synchronous start.")
      (test-assert (eql (getf (slynk-status) :port) 4711)
                   "The status reports the bound port.")
      (test-assert (equal (slynk-test-port-file configuration) (format nil "4711~%"))
                   "The port file holds the bound port.")
      (test-assert (null (slynk-test-stub-value "*GLOBAL-DEBUGGER*"))
                   "Slynk's global debugger hook is off.")
      (test-assert (null (slynk-test-stub-value "*GLOBALLY-REDIRECT-IO*"))
                   "Slynk's global stream redirection is off.")
      (test-assert (typep (slynk-test-stub-value "*LOG-OUTPUT*") 'broadcast-stream)
                   "Slynk logs to a sink instead of the terminal.")
      (test-assert (equal (first (slynk-test-stub-value "*CALLS*"))
                          (list :create 0 t "127.0.0.1"))
                   "The server binds loopback, accepts many connections and picks a port.")
      (test-assert (equal asdf:*central-registry* registry)
                   "No directory joins the ASDF search path.")
      (test-assert (eq (slynk-start configuration) endpoint)
                   "Starting again returns the running endpoint."))))

(defun test-slynk-start-without-directory ()
  "Without a Slynk directory nothing starts and the status stays stopped."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let ((*slynk-endpoint* nil)
          (configuration (slynk-test-configuration root nil)))
      (test-assert (null (slynk-start configuration :background-p t))
                   "Starting without a directory returns NIL.")
      (test-assert (null *slynk-endpoint*)
                   "No endpoint is recorded.")
      (test-assert (eq (getf (slynk-status) :state) ':stopped)
                   "The status reports stopped.")
      (test-assert (null (slynk-test-port-file configuration))
                   "No port file is written."))))

(defun test-slynk-start-failure-reports-port-file ()
  "A failed bind leaves a :FAILED endpoint and tells the editor why."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (slynk-test-write-stub root))
           (configuration (slynk-test-configuration root directory)))
      (asdf:load-asd (merge-pathnames "slynk.asd" directory))
      (asdf:load-system "slynk")
      (setf (symbol-value (find-symbol "*FAIL-P*" '#:slynk)) t)
      (let ((endpoint (slynk-start configuration)))
        (test-assert (eq (slynk-endpoint-state endpoint) ':failed)
                     "A bind failure fails the endpoint.")
        (test-assert (search "address already in use" (slynk-endpoint-message endpoint))
                     "The endpoint keeps the bind failure.")
        (test-assert (uiop:string-prefix-p "error: " (slynk-test-port-file configuration))
                     "The port file tells the editor the start failed.")))))

(defun test-slynk-missing-system-fails ()
  "A directory without slynk.asd fails with a reason instead of loading anything."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (merge-pathnames "empty/" root))
           (configuration (slynk-test-configuration root directory)))
      (ensure-directories-exist directory)
      (let ((endpoint (slynk-start configuration)))
        (test-assert (eq (slynk-endpoint-state endpoint) ':failed)
                     "A missing slynk.asd fails the endpoint.")
        (test-assert (search "No slynk.asd" (slynk-endpoint-message endpoint))
                     "The failure names the missing system.")))))

(defun test-slynk-quiesce-restarts-same-port ()
  "Quiescing closes connections, runs the function stopped and serves the same port again."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (slynk-test-write-stub root))
           (configuration (slynk-test-configuration root directory))
           (state-inside nil))
      (slynk-start configuration)
      (slynk-call-quiesced
       (lambda () (setf state-inside (getf (slynk-status) :state))))
      (test-assert (eq state-inside ':stopped)
                   "The function runs while Slynk is stopped.")
      (test-assert (null (slynk-test-stub-value "*CONNECTIONS*"))
                   "Quiescing closes every sly connection.")
      (test-assert (eq (getf (slynk-status) :state) ':running)
                   "Slynk serves again afterwards.")
      (test-assert (equal (subseq (slynk-test-stub-value "*CALLS*") 0 2)
                          (list (list :create 4711 t "127.0.0.1")
                                (list :stop 4711)))
                   "Slynk stops and binds the same port again."))))

(defun test-slynk-real-server ()
  "The host's real Slynk accepts a loopback connection and stops serving when asked."
  (let ((directory (test-slynk-directory)))
    (if (null directory)
        (test-withheld ':slynk-source "real Slynk serving")
        (with-test-configuration (base root)
          (declare (ignore base))
          (let* ((*slynk-endpoint* nil)
                 (configuration (slynk-test-configuration root directory))
                 (endpoint (slynk-start configuration))
                 (port (slynk-endpoint-port endpoint)))
            (unwind-protect
                 (progn
                   (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                                (format nil "Real Slynk serves: ~A" (slynk-endpoint-message endpoint)))
                   (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                                :type ':stream :protocol ':tcp)))
                     (unwind-protect
                          (test-assert (progn (sb-bsd-sockets:socket-connect
                                               socket #(127 0 0 1) port)
                                              t)
                                       "A loopback client connects to the real Slynk.")
                       (sb-bsd-sockets:socket-close socket))))
              (slynk-stop))
            (test-assert (eq (getf (slynk-status) :state) ':stopped)
                         "Stopping reports stopped."))))))
