;;;; -- Parent Protocol Handshake Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () parent-proto:welcome) protocol-test--welcome))

(defun protocol-test--welcome ()
  "Construct the smallest valid session-service welcome for handshake checks."
  (parent-proto:make-welcome
   :session-id "session-test"
   :features   '("session.v1")
   :limits     (parent-proto:make-limits :max-frame-bytes        4096
                                         :max-value-depth        32
                                         :max-collection-entries 256
                                         :max-in-flight-requests 4)
   :resume     (parent-proto:make-session-page)))

(declaim (ftype (function (&key (:hello parent-proto:envelope) (:ready parent-proto:envelope)
                               (:welcome parent-proto:welcome) (:reason (or null keyword))
                               (:after-ready function)) null)
                protocol-test--handshake))

(defun protocol-test--handshake
    (&key (hello (parent-proto:make-envelope
                  :protocol-major 1 :instance-id "instance-test" :message-id 1
                  :hello (parent-proto:make-hello :features '("session.v1" "unknown.future"))))
          (ready (parent-proto:make-envelope
                  :protocol-major 1 :instance-id "instance-test" :message-id 2
                  :reply-to 1 :ready (parent-proto:make-ready)))
          (welcome (protocol-test--welcome)) reason (after-ready #'identity))
  "Check a complete handshake or require its stable failure REASON."
  (uiop:with-temporary-file (:stream input :element-type '(unsigned-byte 8))
    (uiop:with-temporary-file (:stream output :element-type '(unsigned-byte 8))
      (protocol-write-frame input (cl-protobufs:serialize-to-bytes hello))
      (protocol-write-frame input (cl-protobufs:serialize-to-bytes ready))
      (file-position input 0)
      (let* ((connection (make-instance 'parent-connection :input input :output output
                                                          :instance-id "instance-test"))
             (observed   (handler-case
                             (progn
                               (assert (eq connection (parent-connection-handshake connection welcome)))
                               nil)
                           (parent-protocol-error (condition)
                             (parent-protocol-error-reason condition)))))
        (unwind-protect
             (progn
               (assert (eq reason observed))
               (if reason
                   (progn
                     (assert (eq ':closed (parent-connection-phase connection)))
                     (assert (not (open-stream-p input)))
                     (assert (not (open-stream-p output))))
                   (progn
                     (assert (eq ':ready (parent-connection-phase connection)))
                     (assert (= 2 (parent-connection-received connection)))
                     (assert (= 1 (parent-connection-sent connection)))
                     (assert (= 4096 (parent-proto:limits.max-frame-bytes
                                      (parent-connection-limits connection))))
                     (file-position output 0)
                     (let ((reply (cl-protobufs:deserialize-from-bytes
                                   'parent-proto:envelope (protocol-read-frame output))))
                       (assert (= 1 (parent-proto:envelope.protocol-major reply)))
                       (assert (string= "instance-test" (parent-proto:envelope.instance-id reply)))
                       (assert (= 1 (parent-proto:envelope.message-id reply)))
                       (assert (parent-proto:envelope.has-reply-to reply))
                       (assert (= 1 (parent-proto:envelope.reply-to reply)))
                       (assert (eq 'parent-proto:welcome (parent-proto:envelope.body-case reply)))
                       (assert (equal '("session.v1")
                                      (parent-proto:welcome.features
                                       (parent-proto:envelope.welcome reply)))))
                     (funcall after-ready connection))))
          (parent-connection-close connection)))))
  nil)

(defclass protocol-test-waiting-input (trivial-gray-streams:fundamental-binary-input-stream)
  ()
  (:documentation "A controlled blocking input stream for the whole-handshake deadline check."))

(defmethod trivial-gray-streams:stream-read-byte ((stream protocol-test-waiting-input))
  "Wait long enough to require the handshake deadline to interrupt the read."
  (declare (ignore stream))
  (sleep 60)
  ':eof)

(declaim (ftype (function () null) protocol-test--concurrent-send))

(defun protocol-test--concurrent-send ()
  "Verify concurrent writers produce complete frames with consecutive identifiers."
  (uiop:with-temporary-file (:stream input :element-type '(unsigned-byte 8))
    (uiop:with-temporary-file (:stream output :element-type '(unsigned-byte 8))
      (let* ((connection (make-instance 'parent-connection :input input :output output
                                                          :instance-id "concurrent"))
             (threads    (loop repeat 8 collect
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (loop repeat 32 do
                                (parent-connection-send
                                 connection
                                 (parent-proto:make-envelope :ready (parent-proto:make-ready)))))))))
        (unwind-protect
             (progn
               (mapc #'bordeaux-threads:join-thread threads)
               (assert (= 256 (parent-connection-sent connection)))
               (file-position output 0)
               (loop for identifier from 1 to 256
                     for bytes = (protocol-read-frame output)
                     for envelope = (cl-protobufs:deserialize-from-bytes 'parent-proto:envelope bytes)
                     do (assert (= identifier (parent-proto:envelope.message-id envelope)))
                        (assert (string= "concurrent" (parent-proto:envelope.instance-id envelope)))
                        (assert (eq 'parent-proto:ready (parent-proto:envelope.body-case envelope))))
               (assert (null (protocol-read-frame output))))
          (parent-connection-close connection)))))
  nil)

(declaim (ftype (function () null) run-protocol-handshake-tests))

(defun run-protocol-handshake-tests ()
  "Check feature selection, envelope identity, reply correlation, closure, and deadline."
  (protocol-test--concurrent-send)
  (protocol-test--handshake)
  (protocol-test--handshake
   :after-ready
   (lambda (connection)
     (let* ((input    (parent-connection-input connection))
            (position (file-position input)))
       (protocol-write-frame
        input (cl-protobufs:serialize-to-bytes
               (parent-proto:make-envelope :protocol-major 99 :instance-id "instance-test"
                                             :message-id   3
                                             :session-read (parent-proto:make-session-read :limit 1))))
       (file-position input position)
       (assert (eq ':unsupported-version
                   (handler-case (progn (parent-connection-receive connection) nil)
                     (parent-protocol-error (condition)
                       (parent-protocol-error-reason condition)))))
       (assert (eq ':closed (parent-connection-phase connection))))))
  (dolist (mutation '((:hello :protocol-major 2 :unsupported-version)
                      (:hello :message-id 2 :message-id-discontinuity)
                      (:hello :instance-id "other-instance" :instance-mismatch)
                      (:hello :reply-to 0 :invalid-hello)
                      (:ready :protocol-major 2 :unsupported-version)
                      (:ready :message-id 3 :message-id-discontinuity)
                      (:ready :reply-to 0 :invalid-ready)))
    (destructuring-bind (stage field value reason) mutation
      (let ((message (if (eq stage ':hello)
                         (parent-proto:make-envelope
                          :protocol-major 1 :instance-id "instance-test" :message-id 1
                          :hello (parent-proto:make-hello :features '("session.v1")))
                         (parent-proto:make-envelope
                          :protocol-major 1 :instance-id "instance-test" :message-id 2
                          :reply-to 1 :ready (parent-proto:make-ready)))))
        (ecase field
          (:protocol-major
           (setf (parent-proto:envelope.protocol-major message) value))
          (:message-id
           (setf (parent-proto:envelope.message-id message) value))
          (:instance-id
           (setf (parent-proto:envelope.instance-id message) value))
          (:reply-to
           (setf (parent-proto:envelope.reply-to message) value)))
        (if (eq stage ':hello)
            (protocol-test--handshake :hello message :reason reason)
            (protocol-test--handshake :ready message :reason reason)))))
  (protocol-test--handshake
   :ready  (parent-proto:make-envelope
            :protocol-major 1 :instance-id "instance-test" :message-id 2
            :ready (parent-proto:make-ready))
   :reason ':invalid-ready)
  (let ((welcome (protocol-test--welcome)))
    (setf (parent-proto:welcome.features welcome) '("session.v1" "model.v1"))
    (protocol-test--handshake :welcome welcome :reason ':invalid-welcome))
  (let ((welcome (protocol-test--welcome)))
    (setf (parent-proto:limits.max-value-depth (parent-proto:welcome.limits welcome)) 33)
    (protocol-test--handshake :welcome welcome :reason ':invalid-welcome))
  (uiop:with-temporary-file (:stream output :element-type '(unsigned-byte 8))
    (let* ((input      (make-instance 'protocol-test-waiting-input))
           (connection (make-instance 'parent-connection :input input :output output
                                                        :instance-id "instance-test")))
      (assert (eq ':handshake-timeout
                  (handler-case
                      (progn (parent-connection-handshake connection (protocol-test--welcome)
                                                         :timeout 0.02)
                             nil)
                    (parent-protocol-error (condition)
                      (parent-protocol-error-reason condition)))))
      (assert (eq ':closed (parent-connection-phase connection)))
      (assert (not (open-stream-p input)))
      (assert (not (open-stream-p output)))))
  nil)
