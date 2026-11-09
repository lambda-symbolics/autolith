(in-package #:autolith)

;;;; -- Management REPL Test Support --

(-> management-repl-test-configuration
    (pathname &key (:transport keyword) (:address string) (:port integer)
                   (:timeout integer) (:maximum-output-size integer))
    configuration)
(defun management-repl-test-configuration
    (root &key (transport (if (platform-supports-p *platform* ':local-sockets)
                              ':unix
                              ':tcp))
               (address "127.0.0.1") (port 4141)
               (timeout 1) (maximum-output-size 4096))
  "Return an enabled management configuration rooted under ROOT."
  (configuration-create
   :source-root                            (asdf:system-source-directory :autolith)
   :working-directory                      (asdf:system-source-directory :autolith)
   :management-repl-enabled-p              t
   :management-repl-transport              transport
   :management-repl-unix-socket-path       (merge-pathnames "private/repl.sock" root)
   :management-repl-tcp-address            address
   :management-repl-tcp-port               port
   :management-repl-token-file-path        (merge-pathnames "token" root)
   :management-repl-evaluation-timeout     timeout
   :management-repl-maximum-frame-size     65536
   :management-repl-maximum-source-size    4096
   :management-repl-maximum-output-size    maximum-output-size
   :management-repl-queue-capacity         2
   :management-repl-maximum-clients        2
   :management-repl-authentication-timeout 1))

(-> management-repl-test-write-token (configuration string) null)
(defun management-repl-test-write-token (configuration token)
  "Write TOKEN to CONFIGURATION's private credential file."
  (let ((pathname (config :management-repl-token-file-path configuration)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction       ':output
                            :if-exists       ':supersede
                            :external-format ':utf-8)
      (write-string token stream))
    (platform-make-private *platform* pathname))
  nil)

(-> management-repl-test-connect (configuration string) stream)
(defun management-repl-test-connect (configuration token)
  "Connect to CONFIGURATION's endpoint, authenticate with TOKEN and return the stream."
  (nth-value 1 (eval-connect
                :transport          (config :management-repl-transport configuration)
                :unix-pathname      (config :management-repl-unix-socket-path configuration)
                :tcp-address        (config :management-repl-tcp-address configuration)
                :tcp-port           (config :management-repl-tcp-port configuration)
                :token              token
                :maximum-frame-size 65536
                :timeout            2)))

(-> management-repl-test-free-port () (integer 1 65535))
(defun management-repl-test-free-port ()
  "Return a currently free IPv4 loopback TCP port."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type     ':stream
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
     (equal (config reader left) (config reader right)))
   (list :management-repl-enabled-p           :management-repl-transport
         :management-repl-unix-socket-path    :management-repl-tcp-address
         :management-repl-tcp-port            :management-repl-token-file-path
         :management-repl-evaluation-timeout  :management-repl-maximum-frame-size
         :management-repl-maximum-source-size :management-repl-maximum-output-size
         :management-repl-queue-capacity      :management-repl-maximum-clients
         :management-repl-authentication-timeout)))


;;;; -- Focused Tests --

(-> test-management-repl-configuration () null)
(defun test-management-repl-configuration ()
  "Test secure defaults, explicit settings, cloning, and loopback validation."
  (let* ((root  (uiop:ensure-directory-pathname
                 (merge-pathnames
                  (format nil "management-config-~A/" (make-identifier))
                  (uiop:temporary-directory))))
         (configuration
           (management-repl-test-configuration
            root :transport ':tcp :address "127.0.0.2" :port 4545
            :timeout 7 :maximum-output-size 2048))
         (clone (configuration-copy configuration))
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
            (not (config :management-repl-enabled-p
                  (configuration-create
                   :source-root (asdf:system-source-directory :autolith)
                   :working-directory
                   (asdf:system-source-directory :autolith))))
            "management endpoint is disabled by default")
           (let ((relative
                   (configuration-create
                    :source-root                      (asdf:system-source-directory :autolith)
                    :working-directory
                    (asdf:system-source-directory :autolith)
                    :management-repl-unix-socket-path #P"relative/repl.sock"
                    :management-repl-token-file-path  #P"relative/token")))
             (test-assert
              (and
               (uiop:absolute-pathname-p
                (config :management-repl-unix-socket-path relative))
               (uiop:absolute-pathname-p
                (config :management-repl-token-file-path relative)))
              "management filesystem paths are anchored when configured"))
           (let ((application
                   (make-instance 'application
                                  :configuration
                                  (management-repl-test-configuration
                                   root :transport ':tcp :address "192.0.2.1"))))
             (test-assert
              (handler-case
                  (progn (management-repl-start application) nil)
                (management-repl-error (condition)
                  (and (eq (management-repl-error-reason condition) ':non-loopback)
                       (null (application-management-repl-endpoint application)))))
              "management TCP rejects non-loopback addresses as a configuration error")))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-management-repl-unix-lifecycle () null)
(defun test-management-repl-unix-lifecycle ()
  "Test Unix authentication, active-image evaluation, quiescence, and shutdown."
  (with-platform-capability (':local-sockets "the Unix management transport")
    (management-repl-tests--unix-lifecycle))
  nil)

(-> management-repl-tests--unix-lifecycle () null)
(defun management-repl-tests--unix-lifecycle ()
  "Drive the Unix management transport through its application lifecycle."
  (let* ((root          (test-make-temporary-root))
         (configuration (management-repl-test-configuration root))
         (token         "test-management-token")
         (application   (make-instance 'application :configuration configuration))
         (stream        nil))
    (unwind-protect
         (progn
           (management-repl-test-write-token configuration token)
           (let ((endpoint (management-repl-start application)))
             (test-assert
              (not (test-object-contains-string-p endpoint token))
              "management endpoint retains no raw token")
             (test-assert (eq endpoint (management-repl-start application))
                          "starting a running management endpoint keeps it")
             (setf stream (management-repl-test-connect configuration token))
             (let ((response
                     (eval-call stream
                                "(progn (format t \"hello\") (values 42 (package-name *package*)))"
                                :maximum-frame-size 65536)))
               (test-assert
                (and (eq (getf (rest response) :status) ':ok)
                     (equal (getf (rest response) :values) '("42" "\"AUTOLITH\""))
                     (string= (getf (rest response) :output) "hello"))
                "management evaluation runs in the AUTOLITH package with captured output"))
             (close stream)
             (setf stream nil)
             (test-assert
              (eq (application-call-with-management-repl-quiesced
                   application
                   (lambda ()
                     (and (null (application-management-repl-endpoint application))
                          ':quiesced)))
                  ':quiesced)
              "checkpoint quiescence removes the management endpoint")
             (test-assert
              (let ((restarted (application-management-repl-endpoint application)))
                (and restarted (not (eq restarted endpoint))))
              "checkpoint quiescence restarts a fresh endpoint"))
           (let ((successor (make-instance 'application :configuration configuration))
                 (endpoint  (application-management-repl-endpoint application)))
             (management-repl-transfer application successor)
             (test-assert
              (and (null (application-management-repl-endpoint application))
                   (eq (application-management-repl-endpoint successor) endpoint))
              "reconnect transfers the running endpoint to the new application")
             (management-repl-stop successor)
             (management-repl-stop successor))
           (test-assert
            (not (probe-file
                  (config :management-repl-unix-socket-path configuration)))
            "management shutdown idempotently removes its owned Unix socket"))
      (when stream
        (ignore-errors (close stream)))
      (ignore-errors (management-repl-stop application))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-management-repl-tcp-lifecycle () null)
(defun test-management-repl-tcp-lifecycle ()
  "Test authenticated loopback TCP startup and deterministic shutdown."
  (let* ((root        (uiop:ensure-directory-pathname
                       (merge-pathnames
                        (format nil "management-tcp-~A/" (make-identifier))
                        (uiop:temporary-directory))))
         (configuration
           (management-repl-test-configuration
            root :transport ':tcp :port (management-repl-test-free-port)))
         (application (make-instance 'application :configuration configuration)))
    (unwind-protect
         (progn
           (management-repl-test-write-token configuration "tcp-test-token")
           (management-repl-start application)
           (close (management-repl-test-connect configuration "tcp-test-token"))
           (management-repl-stop application)
           (test-assert (null (application-management-repl-endpoint application))
                        "management TCP shuts down deterministically"))
      (ignore-errors (management-repl-stop application))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)
