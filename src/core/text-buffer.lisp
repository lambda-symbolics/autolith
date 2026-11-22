(in-package #:autolith)

;;;; -- Growable Text Buffers --

(-> text-buffer-create (&key (:capacity (integer 1))) string)
(defun text-buffer-create (&key (capacity 256))
  "Return an empty growable text buffer with initial CAPACITY characters."
  (make-array capacity
              :element-type 'character
              :adjustable   t
              :fill-pointer 0))

(-> text-buffer-append (string string) string)
(defun text-buffer-append (buffer text)
  "Append TEXT to BUFFER in place with amortized growth and return BUFFER."
  (serapeum:vector-conc-extend buffer text (* 2 (array-total-size buffer))))

(-> text-buffer-clear (string) string)
(defun text-buffer-clear (buffer)
  "Empty BUFFER in place, keeping its grown capacity, and return BUFFER."
  (setf (fill-pointer buffer) 0)
  buffer)

(-> text-buffer-string (string) simple-string)
(defun text-buffer-string (buffer)
  "Return BUFFER's current content as a detached simple string."
  (copy-seq buffer))
