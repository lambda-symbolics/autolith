;;;; -- Parent-Agent Framing Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (list) (simple-array (unsigned-byte 8) (*))) protocol-test--bytes))

(defun protocol-test--bytes (bytes)
  "Make a simple octet vector from the test byte list BYTES."
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(declaim (ftype (function (list keyword &key (:limit (integer 1))) null)
                protocol-test--reject-frame))

(defun protocol-test--reject-frame (bytes reason &key (limit *protocol-bootstrap-frame-limit*))
  "Require a frame failure with REASON when reading BYTES under LIMIT."
  (uiop:with-temporary-file (:stream stream :element-type '(unsigned-byte 8))
    (write-sequence (protocol-test--bytes bytes) stream)
    (file-position stream 0)
    (assert (eq reason
                (handler-case
                    (progn (protocol-read-frame stream :limit limit) nil)
                  (protocol-frame-error (condition)
                    (protocol-frame-error-reason condition))))))
  nil)

(declaim (ftype (function () null) run-framing-tests))

(defun run-framing-tests ()
  "Check frame boundaries, byte order, negotiated limits, and partial input."
  (dolist (case '(((0) :truncated-prefix)
                  ((0 0) :truncated-prefix)
                  ((0 0 0) :truncated-prefix)
                  ((0 0 0 0) :empty-frame)
                  ((255 255 255 255) :frame-too-large)
                  ((0 128 0 1) :frame-too-large)
                  ((0 0 0 1) :truncated-body)
                  ((0 0 0 3 10) :truncated-body)
                  ((0 0 0 3 10 20) :truncated-body)))
    (protocol-test--reject-frame (first case) (second case)))
  (protocol-test--reject-frame '(0 0 0 3) ':frame-too-large :limit 2)
  (protocol-test--reject-frame nil ':invalid-limit
                               :limit (1+ *protocol-bootstrap-frame-limit*))
  (let ((bytes (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (index (length bytes))
      (setf (aref bytes index) index))
    (uiop:with-temporary-file (:stream stream :element-type '(unsigned-byte 8))
      (assert (null (protocol-read-frame stream)))
      (protocol-write-frame stream bytes :limit 256)
      (protocol-write-frame stream (protocol-test--bytes '(255)) :limit 1)
      (file-position stream 0)
      (assert (equal '(0 0 1 0) (loop repeat 4 collect (read-byte stream))))
      (file-position stream 0)
      (assert (equalp bytes (protocol-read-frame stream :limit 256)))
      (assert (equalp #(255) (protocol-read-frame stream :limit 1)))
      (assert (null (protocol-read-frame stream)))))
  (uiop:with-temporary-file (:stream stream :element-type '(unsigned-byte 8))
    (dolist (bytes (list (protocol-test--bytes nil) (protocol-test--bytes '(1 2 3))))
      (assert (handler-case
                  (progn (protocol-write-frame stream bytes :limit 2) nil)
                (protocol-frame-error () t)))
      (assert (zerop (file-position stream)))))
  nil)
