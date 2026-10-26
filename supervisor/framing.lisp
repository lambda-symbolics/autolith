;;;; -- Parent-Agent Byte Framing --

(in-package #:autolith-supervisor)

(defparameter *protocol-bootstrap-frame-limit* (* 8 1024 1024)
  "The largest frame permitted before and after limit negotiation.")

(define-condition protocol-frame-error (error)
  ((reason :initarg       :reason
           :reader        protocol-frame-error-reason
           :documentation "The framing failure, without untrusted frame contents."))
  (:documentation "A fatal length-prefix or frame-body failure on a protocol connection.")
  (:report (lambda (condition stream)
             (format stream "Invalid parent-agent frame: ~a."
                     (protocol-frame-error-reason condition)))))

(declaim (ftype (function (stream &key (:limit (integer 1)))
                         (or null (simple-array (unsigned-byte 8) (*))))
                protocol-read-frame))

(defun protocol-read-frame (stream &key (limit *protocol-bootstrap-frame-limit*))
  "Read one bounded frame from binary STREAM, or return NIL at a clean end of input.
Reject partial prefixes and bodies. Validate the declared size before allocation."
  (protocol--validate-frame-limit limit)
  (let ((first-byte (read-byte stream nil nil)))
    (when (null first-byte)
      (return-from protocol-read-frame nil))
    (let ((size first-byte))
      (loop repeat 3 do
        (let ((byte (read-byte stream nil nil)))
          (when (null byte)
            (error 'protocol-frame-error :reason ':truncated-prefix))
          (setf size (logior (ash size 8) byte))))
      (protocol--validate-frame-size size limit)
      (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence bytes stream) size)
          (error 'protocol-frame-error :reason ':truncated-body))
        bytes))))

(declaim (ftype (function (stream (simple-array (unsigned-byte 8) (*))
                           &key (:limit (integer 1))) null)
                protocol-write-frame))

(defun protocol-write-frame (stream bytes &key (limit *protocol-bootstrap-frame-limit*))
  "Write and flush one bounded byte frame to binary STREAM.
Validate the complete size before writing its prefix."
  (protocol--validate-frame-limit limit)
  (let ((size (length bytes)))
    (protocol--validate-frame-size size limit)
    (dolist (shift '(24 16 8 0))
      (write-byte (ldb (byte 8 shift) size) stream))
    (write-sequence bytes stream)
    (finish-output stream))
  nil)

(declaim (ftype (function ((integer 1)) null) protocol--validate-frame-limit))

(defun protocol--validate-frame-limit (limit)
  "Require a positive LIMIT no greater than the bootstrap maximum."
  (unless (<= 1 limit *protocol-bootstrap-frame-limit*)
    (error 'protocol-frame-error :reason ':invalid-limit))
  nil)

(declaim (ftype (function ((unsigned-byte 32) (integer 1)) null)
                protocol--validate-frame-size))

(defun protocol--validate-frame-size (size limit)
  "Require a nonempty frame SIZE within the selected LIMIT."
  (unless (<= 1 size limit)
    (error 'protocol-frame-error :reason (if (zerop size) ':empty-frame ':frame-too-large)))
  nil)
