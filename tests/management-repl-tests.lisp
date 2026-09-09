(in-package #:autolith)

;;;; -- Management REPL Test Support --

(-> management-repl-test-configuration
    (pathname &key (:transport keyword) (:address string) (:port integer)
                   (:timeout integer) (:maximum-output-size integer)
                   (:maximum-clients integer) (:authentication-timeout integer)
                   (:maximum-frame-size integer))
    configuration)
(defun management-repl-test-configuration
    (root &key (transport (if (platform-supports-p *platform* ':local-sockets)
                              ':unix
                              ':tcp))
               (address "127.0.0.1") (port 4141)
               (timeout 1) (maximum-output-size 4096)
               (maximum-clients 2) (authentication-timeout 1)
               (maximum-frame-size 65536))
  "Return an enabled management configuration rooted under ROOT."
  (configuration-create
   :source-root (asdf:system-source-directory :autolith)
   :working-directory (asdf:system-source-directory :autolith)
   :management-repl-enabled-p t
   :management-repl-transport transport
   :management-repl-unix-socket-path (merge-pathnames "private/repl.sock" root)
   :management-repl-tcp-address address
   :management-repl-tcp-port port
   :management-repl-token-file-path (merge-pathnames "token" root)
   :management-repl-evaluation-timeout timeout
   :management-repl-maximum-frame-size maximum-frame-size
   :management-repl-maximum-source-size 4096
   :management-repl-maximum-output-size maximum-output-size
   :management-repl-queue-capacity 2
   :management-repl-maximum-clients maximum-clients
   :management-repl-authentication-timeout authentication-timeout))

(-> management-repl-test-write-token (configuration string) null)
(defun management-repl-test-write-token (configuration token)
  "Write TOKEN to CONFIGURATION's private credential file."
  (let ((pathname (configuration-management-repl-token-file-path configuration)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction ':output
                            :if-exists ':supersede
                            :external-format ':utf-8)
      (write-string token stream))
    (platform-make-private *platform* pathname))
  nil)

(-> management-repl-test-connect (configuration string)
    (values sb-bsd-sockets:socket stream))
(defun management-repl-test-connect (configuration token)
  "Connect and authenticate to CONFIGURATION with TOKEN."
  (let ((socket
          (ecase (configuration-management-repl-transport configuration)
            (:unix
             (platform-connect-local
              *platform*
              (configuration-management-repl-unix-socket-path configuration)))
            (:tcp
             (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                          :type ':stream
                                          :protocol ':tcp)))
               (sb-bsd-sockets:socket-connect
                socket
                (sb-bsd-sockets:make-inet-address
                 (configuration-management-repl-tcp-address configuration))
                (configuration-management-repl-tcp-port configuration))
               socket)))))
    (let* ((maximum
             (configuration-management-repl-maximum-frame-size configuration))
           (stream
             (sb-bsd-sockets:socket-make-stream
              socket
              :input t
              :output t
              :element-type '(unsigned-byte 8)
              :buffering ':none
              :timeout 2))
           (challenge (management-repl-read-frame stream maximum))
           (nonce
             (management-repl--hex->octets
              (getf (rest challenge) :nonce)
              *management-repl-nonce-octets*))
           (token-octets
             (sb-ext:string-to-octets token :external-format ':utf-8))
           (proof (management-repl--hmac token-octets nonce)))
      (unwind-protect
           (progn
             (management-repl-write-frame
              stream
              (list ':authenticate
                    :proof (management-repl--octets->hex proof))
              maximum)
             (test-assert
              (eq (first (management-repl-read-frame stream maximum))
                  ':authenticated)
              "management connections authenticate with nonce HMAC")
             (values socket stream))
        (fill token-octets 0)
        (fill nonce 0)
        (fill proof 0)))))

(-> management-repl-test-free-port () (integer 1 65535))
(defun management-repl-test-free-port ()
  "Return a currently free IPv4 loopback TCP port."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type ':stream
                               :protocol ':tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind
            socket (sb-bsd-sockets:make-inet-address "127.0.0.1") 0)
           (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (sb-bsd-sockets:socket-close socket))))


(-> management-repl-test-same-settings-p
    (configuration configuration)
    boolean)
(defun management-repl-test-same-settings-p (left right)
  "Return true when LEFT and RIGHT have equal management endpoint settings."
  (every
   (lambda (reader)
     (equal (funcall reader left) (funcall reader right)))
   (list #'configuration-management-repl-enabled-p
         #'configuration-management-repl-transport
         #'configuration-management-repl-unix-socket-path
         #'configuration-management-repl-tcp-address
         #'configuration-management-repl-tcp-port
         #'configuration-management-repl-token-file-path
         #'configuration-management-repl-evaluation-timeout
         #'configuration-management-repl-maximum-frame-size
         #'configuration-management-repl-maximum-source-size
         #'configuration-management-repl-maximum-output-size
         #'configuration-management-repl-queue-capacity
         #'configuration-management-repl-maximum-clients
         #'configuration-management-repl-authentication-timeout)))


;;;; -- Focused Tests --

(-> test-management-repl-configuration () null)
(defun test-management-repl-configuration ()
  "Test secure defaults, explicit settings, cloning, and loopback validation."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "management-config-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (configuration
           (management-repl-test-configuration
            root :transport ':tcp :address "127.0.0.2" :port 4545
            :timeout 7 :maximum-output-size 2048))
         (clone (configuration--clone configuration))
         (reconnect
           (application--reconnect-configuration configuration nil)))
    (unwind-protect
         (progn
            (test-assert
             (management-repl-test-same-settings-p configuration clone)
             "configuration clones preserve management endpoint settings")
            (test-assert
             (management-repl-test-same-settings-p configuration reconnect)
             "non-environment reconnect preserves explicit management settings")
           (test-assert
            (not (configuration-management-repl-enabled-p
                  (configuration-create
                   :source-root (asdf:system-source-directory :autolith)
                   :working-directory
                   (asdf:system-source-directory :autolith))))
            "management endpoint is disabled by default")
           (let ((relative
                   (configuration-create
                    :source-root (asdf:system-source-directory :autolith)
                    :working-directory
                    (asdf:system-source-directory :autolith)
                    :management-repl-unix-socket-path #P"relative/repl.sock"
                    :management-repl-token-file-path #P"relative/token")))
             (test-assert
              (and
               (uiop:absolute-pathname-p
                (configuration-management-repl-unix-socket-path relative))
               (uiop:absolute-pathname-p
                (configuration-management-repl-token-file-path relative)))
              "management filesystem paths are anchored when configured"))
           (test-assert
            (handler-case
                (progn
                  (management-repl--make-listener
                   (management-repl-test-configuration
                    root :transport ':tcp :address "192.0.2.1"))
                  nil)
              (management-repl-configuration-error () t))
            "management TCP rejects non-loopback addresses"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-management-repl-protocol () null)
(defun test-management-repl-protocol ()
  "Test bounded framing and safe single-form readers."
  (let ((output (make-in-memory-output-stream)))
    (management-repl-write-frame output '(:evaluate :source "(+ 1 2)") 4096)
    (let ((input
            (flexi-streams:make-in-memory-input-stream
             (get-output-stream-sequence output))))
      (test-assert
       (equal (management-repl-read-frame input 4096)
              '(:evaluate :source "(+ 1 2)"))
       "management framing round-trips one readable S-expression")))
  (test-assert
   (handler-case
       (progn (management-repl--read-source-form "(+ 1 2) (+ 3 4)") nil)
     (management-repl-protocol-error (condition)
       (eq (management-repl-protocol-error-reason condition) ':trailing)))
   "management source rejects trailing forms")
  (test-assert
   (handler-case
       (progn (management-repl--read-source-form "#.(error \"unsafe\")") nil)
     (management-repl-protocol-error () t)
     (reader-error () t))
   "management source disables read-time evaluation")
  (test-assert
   (not (management-repl--constant-time-equal-p
         (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
         (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1)))
   "management authentication rejects unequal proofs")
  nil)

(-> test-management-repl-debugger-hook () null)
(defun test-management-repl-debugger-hook ()
  "Test management evaluation intercepts debugger entry before the process hook."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "management-debugger-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (configuration (management-repl-test-configuration root))
         (application (make-instance 'application :configuration configuration))
         (runtime (make-instance 'management-repl-runtime
                                 :configuration configuration
                                 :application application
                                 :listener nil))
         (request (make-instance 'management-repl-request
                                 :source "(break \"management test\")"
                                 :deadline (+ (get-internal-real-time)
                                              (* 2 internal-time-units-per-second))))
         (outer-hook-called-p nil))
    (let ((sb-ext:*invoke-debugger-hook*
            (lambda (condition hook)
              (declare (ignore condition hook))
              (setf outer-hook-called-p t)
              (error "The process debugger hook was invoked."))))
      (let ((response (management-repl--evaluate-request runtime request)))
        (test-assert
         (and (not outer-hook-called-p)
              (eq (getf (rest response) :status) ':condition)
              (search "management test" (getf (rest response) :report)))
         "management evaluation intercepts BREAK before the process debugger hook"))))
  nil)


(-> management-repl-test-listener-stop () null)
(defun management-repl-test-listener-stop ()
  "Test a shutdown wakeup exits the accept loop before its listener is closed."
  (let ((runtime (make-instance 'management-repl-runtime :listener nil))
        (socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type ':stream
                               :protocol ':tcp))
        (accept-count 0))
    (setf (management-repl-runtime-stopping-p runtime) t)
    (unwind-protect
         (test-call-with-function-replacements
          (list (list 'sb-bsd-sockets:socket-accept
                      (lambda (listener)
                        (declare (ignore listener))
                        (if (= (incf accept-count) 1)
                            socket
                            (throw 'accepted-after-stop nil)))))
          (lambda ()
            (catch 'accepted-after-stop
              (management-repl--serve runtime))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))
    (test-assert (= accept-count 1)
                 "shutdown exits after its wakeup instead of blocking in accept again"))
  nil)

(-> test-management-repl-unix-lifecycle () null)
(defun test-management-repl-unix-lifecycle ()
  "Test Unix authentication, active-image evaluation, quiescence, and shutdown."
  (with-platform-capability (':local-sockets "the Unix management transport")
    (management-repl-tests--unix-lifecycle))
  nil)

(-> management-repl-tests--unix-lifecycle () null)
(defun management-repl-tests--unix-lifecycle ()
  "Drive the Unix management transport through its lifecycle."
  (management-repl-test-listener-stop)
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "management-unix-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (configuration (management-repl-test-configuration root))
         (token "test-management-token")
         (application (make-instance 'application :configuration configuration))
         (stream nil))
    (unwind-protect
         (progn
           (management-repl-test-write-token configuration token)
           (let ((runtime (management-repl-start application)))
             (test-assert
              (not (test-object-contains-string-p runtime token))
              "management runtime retains no raw token")
             (multiple-value-bind (socket connected-stream)
                 (management-repl-test-connect configuration token)
               (declare (ignore socket))
               (setf stream connected-stream)
               (management-repl-write-frame
                stream
                '(:evaluate
                  :source "(progn (format t \"hello\") (values 42 :done))")
                65536)
               (let ((response (management-repl-read-frame stream 65536)))
                 (test-assert
                  (and (eq (getf (rest response) :status) ':ok)
                       (equal (getf (rest response) :values) '("42" ":DONE"))
                       (string= (getf (rest response) :output) "hello"))
                  "management evaluator returns all values and captured output"))
               (close stream)
               (setf stream nil))
             (test-assert
              (eq (application-call-with-management-repl-quiesced
                   application
                   (lambda ()
                     (and (null (application-management-repl-runtime application))
                          ':quiesced)))
                  ':quiesced)
              "checkpoint quiescence removes the management runtime")
             (test-assert (application-management-repl-runtime application)
                          "checkpoint quiescence restarts the endpoint"))
           (management-repl-stop application)
           (management-repl-stop application)
           (test-assert
            (not (probe-file
                  (configuration-management-repl-unix-socket-path configuration)))
            "management shutdown idempotently removes its owned Unix socket"))
      (when stream
        (ignore-errors (close stream)))
      (management-repl-stop application)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-management-repl-tcp-lifecycle () null)
(defun test-management-repl-tcp-lifecycle ()
  "Test authenticated loopback TCP startup and deterministic shutdown."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "management-tcp-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (configuration
           (management-repl-test-configuration
            root :transport ':tcp :port (management-repl-test-free-port)))
         (application (make-instance 'application :configuration configuration))
         (stream nil))
    (unwind-protect
         (progn
           (management-repl-test-write-token configuration "tcp-test-token")
           (management-repl-start application)
           (multiple-value-bind (socket connected-stream)
               (management-repl-test-connect configuration "tcp-test-token")
             (declare (ignore socket))
             (setf stream connected-stream)
             (close stream)
             (setf stream nil))
           (management-repl-stop application)
           (test-assert (null (application-management-repl-runtime application))
                        "management TCP shuts down deterministically"))
      (when stream
        (ignore-errors (close stream)))
      (management-repl-stop application)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-management-repl-start-failure-atomic () null)
(defun test-management-repl-start-failure-atomic ()
  "Test thread creation failure cleans the listener, evaluator, and owned path."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "management-start-failure-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (configuration (management-repl-test-configuration root))
         (application (make-instance 'application :configuration configuration))
         (constructor *management-repl-start-thread-function*)
         (calls 0))
    (unwind-protect
         (let ((*management-repl-start-thread-function*
                 (lambda (&rest arguments)
                   (incf calls)
                   (when (= calls 2)
                     (error "Injected management listener thread failure."))
                   (apply constructor arguments))))
           (test-assert
            (handler-case
                (progn (management-repl-start application) nil)
              (error () t))
            "management startup reports a thread creation failure")
           (test-assert
            (and (null (application-management-repl-runtime application))
                 (not (probe-file
                       (configuration-management-repl-unix-socket-path
                        configuration))))
            "management startup failure removes all partially started state"))
      (ignore-errors (management-repl-stop application))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-management-repl-adversarial-protocol () null)
(defun test-management-repl-adversarial-protocol ()
  "Reject cyclic exact schemas promptly and degrade oversized responses."
  (dolist (tag '(:authenticate :evaluate))
    (let* ((request (if (eq tag ':authenticate)
                        (list tag ':proof "00")
                        (list tag ':source "(+ 1 2)")))
           (tail (last request)))
      (setf (rest tail) request)
      (test-assert
       (sb-ext:with-timeout 1
         (handler-case
             (progn
               (if (eq tag ':authenticate)
                   (management-repl--decode-schema request tag '(:proof))
                   (management-repl--request-source request 4096))
               nil)
           (management-repl-protocol-error () t)))
       "cyclic authentication and evaluation forms are rejected promptly")))
  (dolist (request '((:evaluate :source "1" :source "2")
                     (:evaluate :source "1" :unknown t)
                     (:evaluate)))
    (test-assert
     (handler-case
         (progn (management-repl--request-source request 4096) nil)
       (management-repl-protocol-error () t))
     "management schemas reject duplicate, unknown, and missing keys"))
  (let ((output (make-in-memory-output-stream)))
    (management-repl--write-response
     output
     (list ':evaluation-result ':status ':ok
           ':output (make-string 4096 :initial-element #\x))
     128)
    (let ((input
            (flexi-streams:make-in-memory-input-stream
             (get-output-stream-sequence output))))
      (test-assert
       (equal (management-repl-read-frame input 128)
              '(:protocol-error :reason :oversized))
       "oversized management responses become bounded protocol errors")))
  nil)

(-> test-management-repl-client-bounds () null)
(defun test-management-repl-client-bounds ()
  "Clean up incomplete authentication under the independent client bound."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "management-capacity-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (configuration
           (management-repl-test-configuration
            root :transport ':tcp :port (management-repl-test-free-port)
            :maximum-clients 1 :authentication-timeout 1))
         (application (make-instance 'application :configuration configuration))
         (socket nil)
         (stream nil)
         (overload-socket nil)
         (overload-stream nil))
    (unwind-protect
         (progn
           (management-repl-test-write-token configuration "capacity-token")
           (management-repl-start application)
           (setf socket
                 (make-instance 'sb-bsd-sockets:inet-socket
                                :type ':stream
                                :protocol ':tcp))
           (sb-bsd-sockets:socket-connect
            socket
            (sb-bsd-sockets:make-inet-address "127.0.0.1")
            (configuration-management-repl-tcp-port configuration))
           (setf stream
                 (sb-bsd-sockets:socket-make-stream
                  socket
                  :input t
                  :output t
                  :element-type '(unsigned-byte 8)
                  :buffering ':none
                  :timeout 2))
           (management-repl-read-frame stream 65536)
           (write-sequence #(0 0) stream)
           (force-output stream)
           (setf overload-socket
                 (make-instance 'sb-bsd-sockets:inet-socket
                                :type ':stream
                                :protocol ':tcp))
           (sb-bsd-sockets:socket-connect
            overload-socket
            (sb-bsd-sockets:make-inet-address "127.0.0.1")
            (configuration-management-repl-tcp-port configuration))
           (setf overload-stream
                 (sb-bsd-sockets:socket-make-stream
                  overload-socket
                  :input t
                  :output t
                  :element-type '(unsigned-byte 8)
                  :buffering ':none
                  :timeout 2))
           (test-assert
            (handler-case
                (eq (read-byte overload-stream nil ':end-of-input)
                    ':end-of-input)
              (error () t))
            "connections beyond the client bound are closed without a handler")
           (ignore-errors (close overload-stream))
           (setf overload-stream nil
                 overload-socket nil)
           (test-assert
            (sb-ext:with-timeout 2
              (loop while
                    (with-lock-held
                        ((management-repl-runtime-lock
                          (application-management-repl-runtime application)))
                      (management-repl-runtime-client-sockets
                       (application-management-repl-runtime application)))
                    do (sleep 0.01))
              t)
            "partial authentication is removed after one absolute deadline")
           (close stream)
           (setf stream nil
                 socket nil)
           (multiple-value-bind (authenticated-socket authenticated-stream)
               (management-repl-test-connect configuration "capacity-token")
             (ignore-errors (close authenticated-stream))
             (ignore-errors
               (sb-bsd-sockets:socket-close authenticated-socket)))
           (test-assert
            (sb-ext:with-timeout 3
              (management-repl-stop application)
              t)
            "management stop remains bounded after incomplete authentication"))
      (when overload-stream
        (ignore-errors (close overload-stream)))
      (when overload-socket
        (ignore-errors (sb-bsd-sockets:socket-close overload-socket)))
      (when stream
        (ignore-errors (close stream)))
      (when socket
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (ignore-errors (management-repl-stop application))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> run-management-repl-tests () null)
(defun run-management-repl-tests ()
  "Run focused management endpoint configuration, protocol, and lifecycle tests."
  (test-management-repl-configuration)
  (test-management-repl-protocol)
  (test-management-repl-debugger-hook)
  (test-management-repl-adversarial-protocol)
  (test-management-repl-client-bounds)
  (test-management-repl-unix-lifecycle)
  (test-management-repl-start-failure-atomic)
  (test-management-repl-tcp-lifecycle)
  nil)
