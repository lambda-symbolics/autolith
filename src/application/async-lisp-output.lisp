(in-package #:autolith)

;;;; -- Bounded Asynchronous Lisp Output --

(defparameter *application-async-lisp-output-capture-limit* 16384
  "Maximum retained characters, including the truncation marker.")

(defparameter *application-async-lisp-output-pending-limit* 2048
  "Maximum characters waiting for live presentation.")

(defparameter *application-async-lisp-output-display-interval* 0.25d0
  "Minimum seconds between live output deliveries.")

(defclass application-async-lisp-output-stream
    (sb-gray:fundamental-character-output-stream)
  ((output-function
    :initarg :output-function :accessor application-async-lisp-output-stream-output-function
    :documentation "Optional sink for sanitized, bounded live output chunks.")
   (display-error
    :initform nil :accessor application-async-lisp-output-stream-display-error
    :documentation "First live presentation failure, retained without aborting evaluation.")
   (lock
    :initform (make-lock "async-lisp-output") :reader application-async-lisp-output-stream-lock
    :documentation "Protects captured characters and terminal sequence state.")
   (display-lock
    :initform (make-lock "async-lisp-output-display")
    :reader application-async-lisp-output-stream-display-lock
    :documentation "Serializes callbacks without holding the capture lock.")
   (buffer
    :initform (make-array *application-async-lisp-output-capture-limit* :element-type 'character)
    :reader application-async-lisp-output-stream-buffer
    :documentation "Fixed-capacity retained output prefix.")
   (buffer-length
    :initform 0 :accessor application-async-lisp-output-stream-buffer-length
    :documentation "Number of retained prefix characters.")
   (truncated-p
    :initform nil :accessor application-async-lisp-output-stream-truncated-p
    :documentation "Whether output exceeded the retained prefix capacity.")
   (pending
    :initform (make-array *application-async-lisp-output-pending-limit* :element-type 'character)
    :reader application-async-lisp-output-stream-pending
    :documentation "Fixed-capacity ring of pending display characters.")
   (pending-start
    :initform 0 :accessor application-async-lisp-output-stream-pending-start
    :documentation "First retained ring index.")
   (pending-length
    :initform 0 :accessor application-async-lisp-output-stream-pending-length
    :documentation "Number of occupied ring positions.")
   (pending-omitted
    :initform 0 :accessor application-async-lisp-output-stream-pending-omitted
    :documentation "Saturating count of overwritten pending characters.")
   (escape-state
    :initform nil :accessor application-async-lisp-output-stream-escape-state
    :documentation "Current ANSI sequence state: NIL, :ESCAPE, :CSI, :OSC, or :OSC-ESCAPE.")
   (last-display-time
    :initform 0.0d0 :accessor application-async-lisp-output-stream-last-display-time
    :documentation "Internal time in seconds of the last display delivery.")
   (column
    :initform 0 :accessor application-async-lisp-output-stream-column
    :documentation "Logical output column for FORMAT and FRESH-LINE."))
  (:documentation "Bounded, synchronized Lisp output capture with a throttled live sink."))

(-> application-async-lisp-output-stream-create
    (&key (:output-function (option function))) application-async-lisp-output-stream)
(defun application-async-lisp-output-stream-create (&key output-function)
  "Create a bounded sanitized stream calling OUTPUT-FUNCTION for live chunks."
  (check-type *application-async-lisp-output-capture-limit* (integer 1))
  (check-type *application-async-lisp-output-pending-limit* (integer 1))
  (make-instance 'application-async-lisp-output-stream :output-function output-function))

(-> application-async-lisp-output-stream--safe-character-p (character) boolean)
(defun application-async-lisp-output-stream--safe-character-p (character)
  "Accept printable characters and ordinary layout, excluding C0, DEL, and C1 controls."
  (not (null (or (member character '(#\Newline #\Return #\Tab))
                 (let ((code (char-code character)))
                   (or (<= 32 code 126) (>= code 160)))))))

(defun application-async-lisp-output-stream--capture (stream character)
  "Retain CHARACTER while STREAM's fixed prefix has room; caller holds its lock."
  (unless (application-async-lisp-output-stream-truncated-p stream)
    (let ((length (application-async-lisp-output-stream-buffer-length stream))
          (limit (length (application-async-lisp-output-stream-buffer stream))))
      (if (< length limit)
          (progn
            (setf (aref (application-async-lisp-output-stream-buffer stream) length) character)
            (incf (application-async-lisp-output-stream-buffer-length stream)))
          (setf (application-async-lisp-output-stream-truncated-p stream) t)))))

(defun application-async-lisp-output-stream--pending (stream character)
  "Append CHARACTER to STREAM's bounded pending ring; caller holds its lock."
  (let* ((length (application-async-lisp-output-stream-pending-length stream))
         (start (application-async-lisp-output-stream-pending-start stream))
         (buffer (application-async-lisp-output-stream-pending stream))
         (limit (length buffer)))
    (if (< length limit)
        (progn
          (setf (aref buffer (mod (+ start length) limit)) character)
          (incf (application-async-lisp-output-stream-pending-length stream)))
        (progn
          (setf (aref buffer start) character
                (application-async-lisp-output-stream-pending-start stream) (mod (1+ start) limit))
          (setf (application-async-lisp-output-stream-pending-omitted stream)
                (min most-positive-fixnum
                     (1+ (application-async-lisp-output-stream-pending-omitted stream))))))))

(defun application-async-lisp-output-stream--accept (stream character)
  "Consume CHARACTER without retaining terminal escape sequences; caller holds the lock."
  (case (application-async-lisp-output-stream-escape-state stream)
    (:escape
     (setf (application-async-lisp-output-stream-escape-state stream)
           (case character (#\[ ':csi) (#\] ':osc))))
    (:csi
     (when (<= 64 (char-code character) 126)
       (setf (application-async-lisp-output-stream-escape-state stream) nil)))
    (:osc
     (cond
       ((= (char-code character) 7)
        (setf (application-async-lisp-output-stream-escape-state stream) nil))
       ((char= character #\Escape)
        (setf (application-async-lisp-output-stream-escape-state stream) ':osc-escape))))
    (:osc-escape
     (setf (application-async-lisp-output-stream-escape-state stream)
           (unless (char= character #\\) ':osc)))
    (otherwise
     (cond
       ((char= character #\Escape)
        (setf (application-async-lisp-output-stream-escape-state stream) ':escape))
       ((application-async-lisp-output-stream--safe-character-p character)
        (application-async-lisp-output-stream--capture stream character)
        (application-async-lisp-output-stream--pending stream character)
        (setf (application-async-lisp-output-stream-column stream)
              (case character
                ((#\Newline #\Return) 0)
                (#\Tab
                 (+ (application-async-lisp-output-stream-column stream)
                    (- 8 (mod (application-async-lisp-output-stream-column stream) 8))))
                (otherwise
                 (1+ (application-async-lisp-output-stream-column stream))))))))))

(defun application-async-lisp-output-stream--pending-text (stream)
  "Copy STREAM's bounded pending tail and omission count; caller holds its lock."
  (let* ((length (application-async-lisp-output-stream-pending-length stream))
         (omitted (application-async-lisp-output-stream-pending-omitted stream))
         (prefix (if (plusp omitted) (format nil "[~:D characters omitted]" omitted) ""))
         (result (make-string (+ (length prefix) length)))
         (start (application-async-lisp-output-stream-pending-start stream))
         (buffer (application-async-lisp-output-stream-pending stream)))
    (replace result prefix)
    (loop for index below length
          do (setf (char result (+ (length prefix) index))
                   (aref buffer (mod (+ start index) (length buffer)))))
    result))

(defun application-async-lisp-output-stream--take-pending (stream force-p)
  "Drain pending output when due or FORCE-P; caller holds the capture lock."
  (let ((now (/ (get-internal-real-time) (float internal-time-units-per-second 1.0d0))))
    (when (and (plusp (application-async-lisp-output-stream-pending-length stream))
               (or force-p (>= (- now (application-async-lisp-output-stream-last-display-time stream))
                               *application-async-lisp-output-display-interval*)))
      (prog1 (application-async-lisp-output-stream--pending-text stream)
        (setf (application-async-lisp-output-stream-pending-start stream) 0
              (application-async-lisp-output-stream-pending-length stream) 0
              (application-async-lisp-output-stream-pending-omitted stream) 0
              (application-async-lisp-output-stream-last-display-time stream) now)))))

(defun application-async-lisp-output-stream--display (stream force-p)
  "Deliver an ordered chunk outside the capture lock when due or FORCE-P."
  (with-lock-held ((application-async-lisp-output-stream-display-lock stream))
    (let ((text
            (with-lock-held ((application-async-lisp-output-stream-lock stream))
              (application-async-lisp-output-stream--take-pending stream force-p))))
      (when (and text (application-async-lisp-output-stream-output-function stream))
        (handler-case
            (funcall (application-async-lisp-output-stream-output-function stream) text)
          (error (condition)
            (application-async-lisp-output-note-error stream condition)))))))

(-> application-async-lisp-output-note-error
    (application-async-lisp-output-stream condition) null)
(defun application-async-lisp-output-note-error (stream condition)
  "Retain a presentation failure and disable the broken live sink."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (setf (application-async-lisp-output-stream-output-function stream) nil
          (application-async-lisp-output-stream-display-error stream)
          (or (application-async-lisp-output-stream-display-error stream)
              (sanitize-text (bounded-string (princ-to-string condition) :limit 512)))))
  nil)

(defmethod sb-gray:stream-write-char ((stream application-async-lisp-output-stream) character)
  "Capture CHARACTER and offer a rate-limited live update."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (application-async-lisp-output-stream--accept stream character))
  (application-async-lisp-output-stream--display stream nil)
  character)

(defmethod sb-gray:stream-write-string
    ((stream application-async-lisp-output-stream) string &optional (start 0) end)
  "Capture STRING's selected range without allocating an unbounded copy."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (loop for index from start below (or end (length string))
          do (application-async-lisp-output-stream--accept stream (char string index))))
  (application-async-lisp-output-stream--display stream nil)
  string)

(defmethod sb-gray:stream-line-column ((stream application-async-lisp-output-stream))
  "Return STREAM's synchronized logical output column."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (application-async-lisp-output-stream-column stream)))

(defmethod sb-gray:stream-force-output ((stream application-async-lisp-output-stream))
  "Offer pending output without bypassing the live display rate limit."
  (application-async-lisp-output-stream--display stream nil)
  nil)

(defmethod sb-gray:stream-finish-output ((stream application-async-lisp-output-stream))
  "Offer pending output; the job finalizer explicitly drains any throttled tail."
  (application-async-lisp-output-stream--display stream nil)
  nil)

(-> application-async-lisp-output-text (application-async-lisp-output-stream) string)
(defun application-async-lisp-output-text (stream)
  "Return bounded sanitized output with truncation and presentation failure details."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (let* ((buffer (application-async-lisp-output-stream-buffer stream))
           (capacity (length buffer))
           (marker
             (bounded-string
              (format nil "~A~@[~%[live output failed: ~A]~]"
                      (if (application-async-lisp-output-stream-truncated-p stream)
                          (format nil "~%[output truncated]")
                          "")
                      (application-async-lisp-output-stream-display-error stream))
              :limit capacity))
           (count (min (application-async-lisp-output-stream-buffer-length stream)
                       (- capacity (length marker)))))
      (concatenate 'string (subseq buffer 0 count) marker))))

(defun application-async-lisp-output-flush (stream)
  "Flush STREAM's pending display tail immediately."
  (application-async-lisp-output-stream--display stream t)
  nil)
