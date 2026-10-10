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
(defvar *servers* nil)
(defvar *worker* nil)
(defvar *connection-closed-hook* nil)
(defvar *new-connection-hook* nil)
(defvar *connection-streams*
  (list (cons :first (make-string-output-stream)) (cons :second (make-string-output-stream))))
(defun connection-socket-io (connection) (rest (assoc connection *connection-streams*)))
(defclass channel () ((thread :initarg :thread :reader channel-thread)))
(defgeneric close-channel (channel &key force))
(defmethod close-channel ((channel channel) &key force) (declare (ignore force)) nil)
(defgeneric thread-for-evaluation (connection id))
(defmethod thread-for-evaluation (connection id)
  (declare (ignore connection id)) *worker*)
(defun current-thread () nil)
(defvar sentinel nil)
(defun find-registered (name) (declare (ignore name)) nil)
(defun connection-channels (connection) (declare (ignore connection)) nil)
(defun multithreaded-connection-p (connection) (declare (ignore connection)) t)
(defun mconn.reader-thread (connection) (declare (ignore connection)) nil)
(defun mconn.control-thread (connection) (declare (ignore connection)) nil)
(defun mconn.auto-flush-thread (connection) (declare (ignore connection)) nil)
(defun mconn.indentation-cache-thread (connection) (declare (ignore connection)) nil)
(defun mconn.active-threads (connection) (declare (ignore connection)) (list *worker*))
(defvar *calls* nil)
(defvar *fail-p* nil)
(defun create-server (&key port dont-close interface style)
  (declare (ignore style))
  (push (list :create port dont-close interface) *calls*)
  (when *fail-p* (error \"address already in use\"))
  (let ((bound (if (zerop port) 4711 port)))
    (push (list nil bound nil) *servers*)
    bound))
(defun stop-server (port)
  (push (list :stop port) *calls*)
  (setf *servers* (remove port *servers* :key #'second)))
(defun close-connection (connection condition backtrace)
  (declare (ignore condition backtrace))
  (close (connection-socket-io connection))
  (setf *connections* (remove connection *connections*))
  (dolist (hook *connection-closed-hook*) (funcall hook connection)))
"
  "The stub slynk.lisp text.")

(-> slynk-test-reset-library () null)
(defun slynk-test-reset-library ()
  "Separate stub and real Slynk definitions between cases sharing a test worker."
  (with-lock-held (*slynk-observation-lock*)
    (test-assert (notany #'thread-alive-p *slynk-closed-threads*)
                 "No observed Slynk thread survives a test fixture reset.")
    (setf *slynk-closed-connections* nil
          *slynk-closed-threads* nil))
  (asdf:clear-system "slynk")
  (let ((package (find-package '#:slynk)))
    (when package
      (dolist (user (package-used-by-list package))
        (unuse-package package user))
      (delete-package package)))
  nil)

(-> slynk-test-write-stub (pathname) pathname)
(defun slynk-test-write-stub (root)
  "Write the stub Slynk under ROOT and return its directory."
  (slynk-test-reset-library)
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

(defun test-slynk-load-reentrant-cancellation ()
  "A loading hook that stops or replaces startup cannot bind the cancelled endpoint."
  (with-test-configuration (base root)
    (declare (ignore base))
    (dolist (replace-p '(nil t))
      (let* ((*slynk-endpoint* nil)
             (configuration (slynk-test-configuration root (slynk-test-write-stub root)))
             (original #'slynk--load)
             (first-p t)
             (replacement nil))
        (test-call-with-function-replacements
         (list (list 'slynk--load
                     (lambda (directory)
                       (funcall original directory)
                       (when first-p
                         (setf first-p nil)
                         (slynk-stop)
                         (when replace-p
                           (setf replacement (slynk-start configuration)))))))
         (lambda ()
           (let ((cancelled (slynk-start configuration)))
             (test-assert (eq (slynk-endpoint-state cancelled) ':stopped)
                          "The reentrantly cancelled endpoint never starts serving.")
             (test-assert (not (slynk-endpoint-bound-p cancelled))
                          "The cancelled endpoint never binds a listener.")
             (test-assert (= (count ':create (slynk-test-stub-value "*CALLS*") :key #'first)
                             (if replace-p 1 0))
                          "Only a replacement endpoint may bind after loading.")
             (when replace-p
               (test-assert (eq *slynk-endpoint* replacement)
                            "Loading preserves the replacement endpoint.")))))
        (slynk-stop)))))

(defun test-slynk-independent-listener-stop ()
  "An independently stopped registered listener can be drained and checkpointed."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (configuration (slynk-test-configuration root (slynk-test-write-stub root)))
           (endpoint (slynk-start configuration))
           (called-p nil))
      (slynk--call "STOP-SERVER" (slynk-endpoint-port endpoint))
      (slynk-call-quiesced (lambda () (setf called-p t)))
      (test-assert called-p "Checkpointing does not wait for registration of a vanished listener.")
      (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                   "Checkpointing resumes serving on the same port.")
      (slynk--call "STOP-SERVER" (slynk-endpoint-port endpoint))
      (slynk-stop)
      (test-assert (eq (slynk-endpoint-state endpoint) ':stopped)
                   "Shutdown also handles an independently stopped listener."))))

(defun test-slynk-partial-load-does-not-block-checkpoints ()
  "A package created by a failed load does not imply a working shutdown protocol."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (slynk-test-write-stub root))
           (configuration (slynk-test-configuration root directory))
           (called-p nil))
      (publish-file (merge-pathnames "slynk.lisp" directory)
                    "(defpackage #:slynk (:use #:cl)) (error \"partial library load\")")
      (let ((endpoint (slynk-start configuration)))
        (test-assert (eq (slynk-endpoint-state endpoint) ':failed)
                     "The partial load fails before binding.")
        (test-assert (not (slynk-endpoint-initialized-p endpoint))
                     "The failed package is not mistaken for an initialized adapter.")
        (slynk-call-quiesced (lambda () (setf called-p t)))
        (test-assert called-p "A failed unbound startup does not poison later checkpoints.")
        (test-assert (not (slynk-endpoint-wanted-p endpoint))
                     "A failed startup is not automatically retried after a checkpoint."))
      (publish-file (merge-pathnames "slynk.lisp" directory) *slynk-test-stub-source*)
      ;; The failed load produced a FASL earlier in this same timestamp second.
      (asdf:load-system "slynk" :force t)
      (test-assert (eq (slynk-endpoint-state (slynk-start configuration)) ':running)
                   "Serving can be retried once the library has been repaired.")
      (slynk-stop))))

(-> slynk-test-wait (sb-thread:semaphore) null)
(defun slynk-test-wait (semaphore)
  "Wait for a deterministic test rendezvous, failing rather than hanging."
  (unless (sb-thread:wait-on-semaphore semaphore :timeout 10)
    (error "Slynk test rendezvous timed out."))
  nil)

(-> slynk-test-start-race (configuration symbol boolean) null)
(defun slynk-test-start-race (configuration stage checkpoint-p)
  "Delay startup at STAGE and race shutdown or a checkpoint against it."
  (let* ((entered (sb-thread:make-semaphore))
         (release (sb-thread:make-semaphore))
         (requested (sb-thread:make-semaphore))
         (completed (sb-thread:make-semaphore))
         (served (sb-thread:make-semaphore))
         (original-stage (symbol-function stage))
         (original-serve #'slynk--serve)
         (first-p t)
         (controller nil)
         (failure nil)
         (inside nil))
    (test-call-with-function-replacements
     (list
      (list stage (lambda (argument)
                    (when first-p
                      (setf first-p nil)
                      (sb-thread:signal-semaphore entered)
                      (slynk-test-wait release))
                    (funcall original-stage argument)))
      (list 'slynk--serve
            (lambda (endpoint)
              ;; Test-local endpoint bindings do not automatically enter threads.
              (let ((*slynk-endpoint* endpoint))
                (unwind-protect (funcall original-serve endpoint)
                  (sb-thread:signal-semaphore served))))))
     (lambda ()
       (let ((endpoint (slynk-start configuration :background-p t)))
         (unwind-protect
              (progn
                (slynk-test-wait entered)
                (setf controller
                      (make-thread
                       (lambda ()
                         (let ((*slynk-endpoint* endpoint))
                           (sb-thread:signal-semaphore requested)
                           (unwind-protect
                                (handler-case
                                    (if checkpoint-p
                                        (slynk-call-quiesced
                                         (lambda ()
                                           (setf inside
                                                 (and (eq (getf (slynk-status) :state) ':stopped)
                                                      (null (slynk-test-stub-value "*SERVERS*"))))))
                                        (slynk-stop))
                                  (error (condition) (setf failure condition)))
                             (sb-thread:signal-semaphore completed))))
                       :name "Slynk lifecycle race"))
                (slynk-test-wait requested)
                (test-assert (not (sb-thread:wait-on-semaphore completed :timeout 0.05))
                             "Shutdown/checkpoint waits for the in-flight load or bind.")
                (sb-thread:signal-semaphore release)
                (slynk-test-wait completed)
                (join-thread controller)
                (slynk-test-wait served)
                (test-assert (null failure) (format nil "The lifecycle completes: ~A" failure))
                (if checkpoint-p
                    (progn
                      (test-assert inside "The checkpoint sees no listener, even during startup.")
                      (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                                   "The endpoint restarts after the checkpoint."))
                    (progn
                      (test-assert (eq (slynk-endpoint-state endpoint) ':stopped)
                                   "Shutdown leaves the endpoint stopped.")
                      (test-assert (null (slynk-test-stub-value "*SERVERS*"))
                                   "No listener outlives shutdown."))))
           (sb-thread:signal-semaphore release)
           (when controller (join-thread controller))
           (slynk-stop))))))
  nil)

(defun test-slynk-startup-races ()
  "Loading and binding are serialized against shutdown and checkpoints."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (configuration (slynk-test-configuration root (slynk-test-write-stub root))))
      (dolist (stage '(slynk--load slynk--create-server))
        (dolist (checkpoint-p '(nil t))
          (slynk-test-start-race configuration stage checkpoint-p))))))

(defun test-slynk-scope-cancels-queued-start ()
  "An endpoint scope that exits before its startup acquires the lock cancels binding."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (configuration (slynk-test-configuration root (slynk-test-write-stub root)))
           (original #'slynk--serve)
           (finished (sb-thread:make-semaphore)))
      (test-call-with-function-replacements
       (list (list 'slynk--serve
                   (lambda (endpoint)
                     (unwind-protect (funcall original endpoint)
                       (sb-thread:signal-semaphore finished)))))
       (lambda ()
         ;; Hold the lifecycle lock until scope cleanup cancels the queued worker.
         (with-recursive-lock-held (*slynk-lock*)
           (catch 'scope-exit
             (with-slynk-endpoint (configuration)
               (throw 'scope-exit t)))
           (test-assert (eq (slynk-endpoint-state *slynk-endpoint*) ':stopped)
                        "Scope unwind cancels startup before releasing the lock."))
         (slynk-test-wait finished)
         (test-assert (null (slynk-test-port-file configuration))
                      "The cancelled worker never loads or publishes a port."))))))

(defun test-slynk-checkpoint-defers-start-and-restarts-after-unwind ()
  "Reentrant starts defer and concurrent starts wait, including a checkpoint unwind."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (configuration (slynk-test-configuration root (slynk-test-write-stub root)))
           (endpoint (slynk-start configuration))
           (entered (sb-thread:make-semaphore))
           (completed (sb-thread:make-semaphore))
           (contender nil)
           (result nil))
      (unwind-protect
           (progn
             (test-assert
              (eq (catch 'checkpoint-exit
                    (slynk-call-quiesced
                     (lambda ()
                       (test-assert (eq (slynk-start configuration) endpoint)
                                    "A reentrant start returns the deferred endpoint.")
                       (test-assert (null (slynk-test-stub-value "*SERVERS*"))
                                    "A reentrant start cannot bind inside the checkpoint.")
                       (slynk-call-quiesced
                        (lambda () (test-assert (eq (getf (slynk-status) :state) ':stopped)
                                               "Nested quiescence also observes stopped.")))
                       (setf contender
                             (make-thread
                              (lambda ()
                                (let ((*slynk-endpoint* endpoint))
                                  (sb-thread:signal-semaphore entered)
                                  (setf result (slynk-start configuration))
                                  (sb-thread:signal-semaphore completed)))
                              :name "Slynk concurrent start"))
                       (slynk-test-wait entered)
                       (test-assert (not (sb-thread:wait-on-semaphore completed :timeout 0.05))
                                    "Concurrent start waits until checkpoint unwind finishes.")
                       (throw 'checkpoint-exit ':unwound))))
                  ':unwound)
              "The checkpoint preserves its non-local exit.")
             (slynk-test-wait completed)
             (join-thread contender)
             (test-assert (eq result endpoint) "Concurrent start returns the resumed endpoint.")
             (test-assert (eq (getf (slynk-status) :state) ':running)
                          "Unwinding restarts the original port.")
             (slynk-call-quiesced #'slynk-stop)
             (test-assert (eq (getf (slynk-status) :state) ':stopped)
                          "An explicit reentrant stop suppresses checkpoint restart."))
        (when contender (join-thread contender))
        (slynk-stop)))))

(defun test-slynk-publication-failure-drains-listener ()
  "A port-file failure after binding does not leak the newly created listener."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (configuration (slynk-test-configuration root (slynk-test-write-stub root))))
      (test-call-with-function-replacements
       (list (list 'slynk--publish (lambda (endpoint text)
                                   (declare (ignore endpoint text))
                                   (error "port publication failed"))))
       (lambda ()
         (let ((endpoint (slynk-start configuration)))
           (test-assert (eq (slynk-endpoint-state endpoint) ':failed)
                        "Publication failure fails startup.")
           (test-assert (search "port publication failed" (slynk-endpoint-message endpoint))
                        "The original publication error is retained.")
           (test-assert (null (slynk-test-stub-value "*SERVERS*"))
                        "The bound listener was stopped.")
           (test-assert (not (slynk-endpoint-bound-p endpoint))
                        "The endpoint records completed listener cleanup.")))))))

(defun test-slynk-quiescence-timeout-and-closed-channel ()
  "Observe disconnected workers, closed channels and workers removed before final unwind."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (*slynk-quiesce-seconds* 0.05)
           (configuration (slynk-test-configuration root (slynk-test-write-stub root)))
           (endpoint (slynk-start configuration)))
      (dolist (mode '(:connection :channel :evaluation :unregistered))
        (let* ((release (sb-thread:make-semaphore))
               (worker (make-thread (lambda () (slynk-test-wait release))
                                    :name "renamed personal evaluation"))
               (called-p nil))
          (unwind-protect
               (progn
                 (ecase mode
                   (:connection
                    (setf (symbol-value (slynk--symbol "*WORKER*")) worker)
                    (slynk--call "CLOSE-CONNECTION" ':first nil nil)
                    (slynk--call "CLOSE-CONNECTION" ':second nil nil))
                   (:channel
                    (slynk--call "CLOSE-CHANNEL"
                                 (make-instance (slynk--symbol "CHANNEL") :thread worker)
                                 :force t))
                  (:unregistered
                   (setf (symbol-value (slynk--symbol "*WORKER*")) worker
                         (symbol-value (slynk--symbol "*CONNECTION-STREAMS*"))
                         (list (cons ':first (make-string-output-stream))))
                   (dolist (hook (symbol-value (slynk--symbol "*NEW-CONNECTION-HOOK*")))
                     (funcall hook ':first)))
                   (:evaluation
                    (setf (symbol-value (slynk--symbol "*WORKER*")) worker)
                    (test-assert (eq (slynk--call "THREAD-FOR-EVALUATION" ':connection t) worker)
                                 "The evaluation observer preserves dispatch's thread result.")
                    ;; Simulate removal from the active list before abort flushes user streams.
                    (setf (symbol-value (slynk--symbol "*WORKER*")) nil)))
                 (test-assert (null (slynk-test-stub-value "*CONNECTIONS*"))
                              "A disappeared connection alone cannot establish quiescence.")
                 (handler-case
                     (slynk-call-quiesced (lambda () (setf called-p t)))
                   (slynk-error (condition)
                     (test-assert (eq (slynk-error-reason condition) ':timeout)
                                  (format nil "~A shutdown times out with the thread alive." mode))))
                 (when (eq mode ':unregistered)
                   (test-assert (not (open-stream-p (slynk--call "CONNECTION-SOCKET-IO" ':first)))
                                "Shutdown closes an accepted connection absent from the sentinel registry."))
                 (test-assert (not called-p) "No checkpoint executes after a drain timeout.")
                 (test-assert (eq (slynk-endpoint-state endpoint) ':stopping)
                              "Incomplete shutdown is not reported as a running listener.")
                 (sb-thread:signal-semaphore release)
                 (join-thread worker)
                 (slynk-call-quiesced (lambda () (setf called-p t)))
                 (test-assert called-p "Draining can be retried after the evaluation exits.")
                 (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                              "Successful retry restarts serving."))
            (sb-thread:signal-semaphore release)
            (join-thread worker)
            (setf (symbol-value (slynk--symbol "*WORKER*")) nil))))
      (slynk-stop))))

(defun test-slynk-unsupported-shutdown-protocol ()
  "An incompatible Slynk fails before any listener can bind."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (slynk-test-write-stub root))
           (configuration (slynk-test-configuration root directory)))
      (slynk--load directory)
      (test-call-with-function-replacements
       (list (list (slynk--symbol "CLOSE-CHANNEL") (lambda (channel &key force)
                                                   (declare (ignore channel force)))))
       (lambda ()
         (let ((endpoint (slynk-start configuration)))
           (test-assert (eq (slynk-endpoint-state endpoint) ':failed)
                        "An incompatible close protocol fails startup.")
           (test-assert (null (slynk-test-stub-value "*SERVERS*"))
                        "Protocol incompatibility is detected before binding.")))))))

(defvar *slynk-test-real-entered* nil
  "Rendezvous signalled by a real Slynk evaluation in the lifecycle test.")

(defvar *slynk-test-real-release* nil
  "Rendezvous releasing the real Slynk evaluation in the lifecycle test.")

(defun test-slynk-real-evaluation-quiescence ()
  "A real disconnected Slynk evaluation blocks checkpointing until it has exited."
  (let ((directory (test-slynk-directory)))
    (if (null directory)
        (test-withheld ':slynk-source "real Slynk evaluation quiescence")
        (with-test-configuration (base root)
          (declare (ignore base))
          (slynk-test-reset-library)
          (let* ((*slynk-endpoint* nil)
                 (configuration (slynk-test-configuration root directory))
                 (endpoint (slynk-start configuration))
                 (socket (make-instance 'sb-bsd-sockets:inet-socket
                                        :type ':stream :protocol ':tcp))
                 (stream nil)
                 (worker nil)
                 (called-p nil))
            (setf *slynk-test-real-entered* (sb-thread:make-semaphore)
                  *slynk-test-real-release* (sb-thread:make-semaphore))
            (unwind-protect
                 (progn
                   (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                                (format nil "Real Slynk starts: ~A" (slynk-endpoint-message endpoint)))
                   (sb-bsd-sockets:socket-connect socket #(127 0 0 1) (slynk-endpoint-port endpoint))
                   (setf stream (sb-bsd-sockets:socket-make-stream
                                 socket :input t :output t :element-type '(unsigned-byte 8)
                                 :buffering ':none))
                   (slynk--call
                    "ENCODE-MESSAGE"
                    (list ':emacs-rex
                          (list (slynk--symbol "INTERACTIVE-EVAL")
                                "(progn (setf (sb-thread:thread-name sb-thread:*current-thread*) \"renamed real evaluation\") (sb-thread:signal-semaphore autolith::*slynk-test-real-entered*) (sb-thread:wait-on-semaphore autolith::*slynk-test-real-release*) 42)")
                          "CL-USER" t 1)
                    stream)
                   (slynk-test-wait *slynk-test-real-entered*)
                   (setf worker (first (slynk--call
                                        "MCONN.ACTIVE-THREADS"
                                        (first (symbol-value (slynk--symbol "*CONNECTIONS*"))))))
                   (close stream)
                   (setf stream nil)
                   (slynk--wait (lambda () (null (symbol-value (slynk--symbol "*CONNECTIONS*"))))
                                "test client disconnect")
                   (test-assert (and worker (thread-alive-p worker))
                                "The real evaluation continues after the connection disappears.")
                   (let ((*slynk-quiesce-seconds* 0.1))
                     (handler-case
                         (slynk-call-quiesced (lambda () (setf called-p t)))
                       (slynk-error (condition)
                         (test-assert (eq (slynk-error-reason condition) ':timeout)
                                      "Real disconnected evaluation fails checkpointing closed."))))
                   (test-assert (not called-p) "No checkpoint ran with the real worker alive.")
                   (sb-thread:signal-semaphore *slynk-test-real-release*)
                   (slynk--wait (lambda () (not (thread-alive-p worker))) "test evaluation exit")
                   (slynk-call-quiesced (lambda () (setf called-p t)))
                   (test-assert called-p "Checkpointing succeeds after the real evaluation exits.")
                   (test-assert (eq (slynk-endpoint-state endpoint) ':running)
                                "Real Slynk restarts on the same port after a successful retry."))
              (sb-thread:signal-semaphore *slynk-test-real-release*)
              (when stream (ignore-errors (close stream)))
              (ignore-errors (sb-bsd-sockets:socket-close socket))
              (slynk-stop)))))))

(defun test-slynk-real-server ()
  "The host's real Slynk accepts a loopback connection and stops serving when asked."
  (let ((directory (test-slynk-directory)))
    (if (null directory)
        (test-withheld ':slynk-source "real Slynk serving")
        (with-test-configuration (base root)
          (declare (ignore base))
          (slynk-test-reset-library)
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


;;;; -- Request Context --

(defun test-slynk-context-note ()
  "The agent hears about the shared image only while Slynk serves."
  (with-test-configuration (base root)
    (declare (ignore base))
    (let* ((*slynk-endpoint* nil)
           (directory (slynk-test-write-stub root))
           (configuration (slynk-test-configuration root directory)))
      (test-assert (null (slynk-shared-image nil))
                   "No note while Slynk is stopped.")
      (slynk-start configuration)
      (let ((note (slynk-shared-image nil)))
        (test-assert (equal (context-contribution-identifier note) "slynk-shared-image")
                     "A note appears while Slynk serves.")
        (test-assert (search "127.0.0.1:4711" (context-contribution-evidence note))
                     "The note names the Slynk port.")
        (test-assert (search "target \"self\"" (context-contribution-instruction note))
                     "The note points at the active-image tools."))
      (slynk-stop)
      (test-assert (null (slynk-shared-image nil))
                   "The note disappears once Slynk stops."))))
