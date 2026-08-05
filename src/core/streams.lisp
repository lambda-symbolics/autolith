(in-package #:autolith)

;;;; -- Bounded Character Reads --

(defparameter *character-read-sequence-window* 256
  "The largest single READ-SEQUENCE character request Autolith issues.

SBCL 2.6.7 introduced SIMD utf-8 decoding that can overrun destination
strings when one request asks for more than 256 characters at once. Requests
at or below 256 characters stay on the portable buffered path on every
supported runtime.")

(-> read-character-sequence (string stream) (integer 0))
(defun read-character-sequence (buffer stream)
  "Fill BUFFER from STREAM like READ-SEQUENCE using bounded requests."
  (let ((length (length buffer))
        (filled 0))
    (loop
      (when (= filled length)
        (return filled))
      (let ((position
              (read-sequence buffer stream
                             :start filled
                             :end (min length
                                       (+ filled
                                          *character-read-sequence-window*)))))
        (when (= position filled)
          (return filled))
        (setf filled position)))))
