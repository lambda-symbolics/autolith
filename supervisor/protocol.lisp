;;;; -- Parent Protocol Connection --

(in-package #:autolith-supervisor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:parent-proto '#:cl-protobufs.autolith.parent.v1
                                     '#:autolith-supervisor))

(defclass parent-connection ()
  ((input       :initarg       :input
                :reader        parent-connection-input
                :type          stream
                :documentation "The private binary input stream from the agent.")
   (output      :initarg       :output
                :reader        parent-connection-output
                :type          stream
                :documentation "The private binary output stream to the agent.")
   (instance-id :initarg       :instance-id
                :reader        parent-connection-instance-id
                :type          string
                :documentation "The fresh instance identifier assigned before launch.")
   (phase       :initform      ':new
                :accessor      parent-connection-phase
                :documentation "The handshake phase, or CLOSED after a fatal failure.")
   (write-lock  :initform      (bordeaux-threads:make-lock "parent protocol writer")
                :reader        parent-connection-write-lock
                :documentation "Serialize parent frame writes and message identifiers.")
   (sent        :initform      0
                :accessor      parent-connection-sent
                :documentation "The last successfully sent parent message identifier.")
   (received    :initform      0
                :accessor      parent-connection-received
                :documentation "The last accepted agent message identifier.")
   (limits      :initform      (parent-proto:make-limits :max-frame-bytes      8388608
                                                       :max-value-depth        32
                                                       :max-collection-entries 65536
                                                       :max-in-flight-requests 64)
                :accessor      parent-connection-limits
                :documentation "The active decode and request limits for this connection."))
  (:documentation "One parent-owned private protocol channel, independent of agent code."))

(define-condition parent-protocol-error (error)
  ((reason :initarg       :reason
           :reader        parent-protocol-error-reason
           :documentation "A stable local failure category without agent-supplied text."))
  (:documentation "A fatal parent-agent protocol or handshake failure.")
  (:report (lambda (condition stream)
             (format stream "Parent-agent protocol failed: ~a."
                     (parent-protocol-error-reason condition)))))

(declaim (ftype (function (parent-connection) null) parent-connection-close))

(defun parent-connection-close (connection)
  "Close CONNECTION's streams and mark it unusable. The launch owner stops the process tree."
  (setf (parent-connection-phase connection) ':closed)
  (unwind-protect
       (close (parent-connection-input connection) :abort t)
    (close (parent-connection-output connection) :abort t))
  nil)

(declaim (ftype (function (parent-connection)
                         (values parent-proto:envelope (simple-array (unsigned-byte 8) (*))))
                parent-connection-receive))

(defun parent-connection-receive (connection)
  "Return a verified envelope and its original frame bytes as a second value.
Validate the active bounds, instance, version, and consecutive message identifier.
Retain the bytes when applying newly negotiated limits; re-encoding drops unknown fields."
  (when (eq (parent-connection-phase connection) ':closed)
    (error 'parent-protocol-error :reason ':closed))
  (handler-case
      (let* ((limits (parent-connection-limits connection))
             (bytes  (protocol-read-frame
                      (parent-connection-input connection)
                      :limit (parent-proto:limits.max-frame-bytes limits))))
        (unless bytes
          (error 'parent-protocol-error :reason ':unexpected-eof))
        (protocol-validate-protobuf
         bytes 'parent-proto:envelope
         :max-bytes   (parent-proto:limits.max-frame-bytes limits)
         :max-depth   (parent-proto:limits.max-value-depth limits)
         :max-entries (parent-proto:limits.max-collection-entries limits))
        (let ((envelope (cl-protobufs:deserialize-from-bytes 'parent-proto:envelope bytes)))
          (unless (string= (parent-connection-instance-id connection)
                           (parent-proto:envelope.instance-id envelope))
            (error 'parent-protocol-error :reason ':instance-mismatch))
          (unless (= (1+ (parent-connection-received connection))
                     (parent-proto:envelope.message-id envelope))
            (error 'parent-protocol-error :reason ':message-id-discontinuity))
          (unless (= 1 (parent-proto:envelope.protocol-major envelope))
            (error 'parent-protocol-error :reason ':unsupported-version))
          (incf (parent-connection-received connection))
          (values envelope bytes)))
    (error (condition)
      (parent-connection-close connection)
      (error condition))))

(declaim (ftype (function (parent-connection parent-proto:envelope) null) parent-connection-send))

(defun parent-connection-send (connection envelope)
  "Serialize parent metadata assignment and the complete bounded frame write."
  (bordeaux-threads:with-lock-held ((parent-connection-write-lock connection))
    (when (eq (parent-connection-phase connection) ':closed)
      (error 'parent-protocol-error :reason ':closed))
    (handler-case
        (let ((limits (parent-connection-limits connection)))
          (setf (parent-proto:envelope.protocol-major envelope) 1
                (parent-proto:envelope.instance-id envelope) (parent-connection-instance-id connection)
                (parent-proto:envelope.message-id envelope) (1+ (parent-connection-sent connection)))
          (let ((bytes (cl-protobufs:serialize-to-bytes envelope)))
            (protocol-validate-protobuf
             bytes 'parent-proto:envelope
             :max-bytes   (parent-proto:limits.max-frame-bytes limits)
             :max-depth   (parent-proto:limits.max-value-depth limits)
             :max-entries (parent-proto:limits.max-collection-entries limits))
            (protocol-write-frame (parent-connection-output connection) bytes
                                  :limit (parent-proto:limits.max-frame-bytes limits)))
          (incf (parent-connection-sent connection)))
      (error (condition)
        (parent-connection-close connection)
        (error condition))))
  nil)

(declaim (ftype (function (parent-connection parent-proto:welcome &key (:timeout (real (0))))
                         parent-connection)
                parent-connection-handshake))

(defun parent-connection-handshake (connection welcome &key (timeout 30))
  "Complete Hello/Welcome/Ready under one TIMEOUT and return CONNECTION.
WELCOME describes the services installed by the caller. On failure, close both
streams. The launch owner must then stop and reap the agent and its descendants."
  (unless (eq (parent-connection-phase connection) ':new)
    (error 'parent-protocol-error :reason ':handshake-already-started))
  (handler-case
      (sb-ext:with-timeout timeout
        (let ((hello (parent-connection-receive connection)))
          (unless (and (= 1 (parent-proto:envelope.protocol-major hello))
                       (eq 'parent-proto:hello (parent-proto:envelope.body-case hello))
                       (not (parent-proto:envelope.has-reply-to hello)))
            (parent-connection--reject-handshake connection hello ':invalid-hello))
          (parent-connection--validate-welcome
           welcome (parent-proto:hello.features (parent-proto:envelope.hello hello)))
          ;; Apply the lower limits before emitting Welcome, including its resume page.
          (setf (parent-connection-limits connection) (parent-proto:welcome.limits welcome)
                (parent-connection-phase connection) ':awaiting-ready)
          (parent-connection-send
           connection (parent-proto:make-envelope :reply-to (parent-proto:envelope.message-id hello)
                                                   :welcome welcome))
          (let ((ready (parent-connection-receive connection)))
            (unless (and (= 1 (parent-proto:envelope.protocol-major ready))
                         (eq 'parent-proto:ready (parent-proto:envelope.body-case ready))
                         (parent-proto:envelope.has-reply-to ready)
                         (= (parent-connection-sent connection)
                            (parent-proto:envelope.reply-to ready)))
              (parent-connection--reject-handshake connection ready ':invalid-ready)))
          (setf (parent-connection-phase connection) ':ready)
          connection))
    (sb-ext:timeout ()
      (parent-connection-close connection)
      (error 'parent-protocol-error :reason ':handshake-timeout))
    (error (condition)
      (parent-connection-close connection)
      (error condition))))

(declaim (ftype (function (parent-proto:welcome list) null) parent-connection--validate-welcome))

(defun parent-connection--validate-welcome (welcome offered)
  "Require a valid parent WELCOME whose selected features are in OFFERED."
  (let ((selected (parent-proto:welcome.features welcome))
        (limits   (parent-proto:welcome.limits welcome)))
    (unless (and (plusp (length (parent-proto:welcome.session-id welcome)))
                 (every (lambda (feature) (plusp (length feature))) offered)
                 (= (length offered) (length (remove-duplicates offered :test #'string=)))
                 (= (length selected) (length (remove-duplicates selected :test #'string=)))
                 (member "session.v1" selected :test #'string=)
                 (subsetp selected offered :test #'string=)
                 (subsetp selected '("session.v1" "model.v1" "records.v1" "access.v1"
                                     "model-items-v1") :test #'string=)
                 (or (not (member "model-items-v1" selected :test #'string=))
                     (member "model.v1" selected :test #'string=))
                 limits
                 (parent-proto:welcome.resume welcome)
                 (<= 1 (parent-proto:limits.max-frame-bytes limits) 8388608)
                 (<= 1 (parent-proto:limits.max-value-depth limits) 32)
                 (<= 1 (parent-proto:limits.max-collection-entries limits) 65536)
                 (<= 1 (parent-proto:limits.max-in-flight-requests limits) 64))
      (error 'parent-protocol-error :reason ':invalid-welcome)))
  nil)

(declaim (ftype (function (parent-connection parent-proto:envelope keyword) null)
                parent-connection--reject-handshake))

(defun parent-connection--reject-handshake (connection request reason)
  "Reply to a semantically invalid handshake REQUEST and signal redacted REASON."
  (parent-connection-send
   connection (parent-proto:make-envelope
               :reply-to (parent-proto:envelope.message-id request)
               :failure  (parent-proto:make-failure :code     ':invalid-request
                                                     :message "Invalid startup message.")))
  (error 'parent-protocol-error :reason reason))
