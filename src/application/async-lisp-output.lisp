(in-package #:autolith)

;;;; -- Bounded Asynchronous Lisp Output --

(defparameter *application-async-lisp-output-capture-limit* 16384
  "Maximum retained characters, including the truncation marker.")

(defparameter *application-async-lisp-output-pending-limit* 2048
  "Maximum characters waiting for live presentation.")

(defparameter *application-async-lisp-output-display-interval* 0.25d0
  "Minimum seconds between live output deliveries.")

(defclass application-async-lisp-output-stream (bounded-output-stream)
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
    :initform      (make-lock "async-lisp-output-display")
    :reader        application-async-lisp-output-stream-display-lock
    :documentation "Serializes callbacks without holding the capture lock.")
   (pending
    :initarg       :pending
    :reader        application-async-lisp-output-stream-pending
    :documentation "Bounded deque of pending display characters, evicting the oldest.")
   (pending-omitted
    :initform 0 :accessor application-async-lisp-output-stream-pending-omitted
    :documentation "Saturating count of evicted pending characters.")
   (ansi-parser
    :initform (colorist:make-ansi-parser) :reader application-async-lisp-output-stream-ansi-parser
    :documentation "Incremental ANSI parser used while the capture lock is held.")
   (last-display-time
    :initform 0.0d0 :accessor application-async-lisp-output-stream-last-display-time
    :documentation "Internal time in seconds of the last display delivery."))
  (:documentation "Bounded, synchronized Lisp output capture with a throttled live sink."))

(-> application-async-lisp-output-stream-create
    (&key (:output-function (option function))) application-async-lisp-output-stream)
(defun application-async-lisp-output-stream-create (&key output-function)
  "Create a bounded sanitized stream calling OUTPUT-FUNCTION for live chunks."
  (check-type *application-async-lisp-output-capture-limit* (integer 1))
  (check-type *application-async-lisp-output-pending-limit* (integer 1))
  (make-instance 'application-async-lisp-output-stream
                 :output-function output-function
                 :builder         (bounded-output-stream--make-builder
                                   *application-async-lisp-output-capture-limit*)
                 :pending         (make-deque :maximum-count *application-async-lisp-output-pending-limit*)))

(-> application-async-lisp-output-stream--safe-character-p (character) boolean)
(defun application-async-lisp-output-stream--safe-character-p (character)
  "Accept printable characters and ordinary layout, excluding C0, DEL, and C1 controls."
  (not (null (or (member character '(#\Newline #\Return #\Tab))
                 (let ((code (char-code character)))
                   (or (<= 32 code 126) (>= code 160)))))))

(-> application-async-lisp-output-stream--pending
    (application-async-lisp-output-stream character) null)
(defun application-async-lisp-output-stream--pending (stream character)
  "Append CHARACTER to STREAM's pending deque, counting what it evicts; caller holds its lock."
  (let ((evicted (nth-value 1 (deque-push-back
                               (application-async-lisp-output-stream-pending stream)
                               character))))
    (when (plusp (length evicted))
      (setf (application-async-lisp-output-stream-pending-omitted stream)
            (min most-positive-fixnum
                 (+ (application-async-lisp-output-stream-pending-omitted stream)
                    (length evicted))))))
  nil)

(-> application-async-lisp-output-stream--accept
    (application-async-lisp-output-stream character) null)
(defun application-async-lisp-output-stream--accept (stream character)
  "Consume CHARACTER without retaining terminal escape sequences; caller holds the lock."
  (when (and (colorist:ansi-parser-accept-p (application-async-lisp-output-stream-ansi-parser stream) character)
             (application-async-lisp-output-stream--safe-character-p character))
    (bounded-output-stream--capture stream character)
    (application-async-lisp-output-stream--pending stream character))
  nil)

(-> application-async-lisp-output-stream--pending-text
    (application-async-lisp-output-stream) string)
(defun application-async-lisp-output-stream--pending-text (stream)
  "Copy STREAM's bounded pending tail and omission count; caller holds its lock."
  (let ((omitted (application-async-lisp-output-stream-pending-omitted stream)))
    (concatenate 'string
                 (if (plusp omitted) (format nil "[~:D characters omitted]" omitted) "")
                 (coerce (deque->vector (application-async-lisp-output-stream-pending stream))
                         'string))))

(-> application-async-lisp-output-stream--take-pending
    (application-async-lisp-output-stream boolean) (option string))
(defun application-async-lisp-output-stream--take-pending (stream force-p)
  "Drain pending output when due or FORCE-P; caller holds the capture lock."
  (let ((now (internal-real-time-seconds)))
    (when (and (plusp (deque-count (application-async-lisp-output-stream-pending stream)))
               (or force-p (>= (- now (application-async-lisp-output-stream-last-display-time stream))
                               *application-async-lisp-output-display-interval*)))
      (prog1 (application-async-lisp-output-stream--pending-text stream)
        (deque-clear (application-async-lisp-output-stream-pending stream))
        (setf (application-async-lisp-output-stream-pending-omitted stream) 0
              (application-async-lisp-output-stream-last-display-time stream) now)))))

(-> application-async-lisp-output-stream--display
    (application-async-lisp-output-stream boolean) null)
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
            (application-async-lisp-output-note-error stream condition))))))
  nil)

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

(defmethod trivial-gray-streams:stream-write-char ((stream application-async-lisp-output-stream) character)
  "Capture CHARACTER and offer a rate-limited live update."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (application-async-lisp-output-stream--accept stream character))
  (application-async-lisp-output-stream--display stream nil)
  character)

(defmethod trivial-gray-streams:stream-write-string
    ((stream application-async-lisp-output-stream) string &optional (start 0) end)
  "Capture STRING's selected range without allocating an unbounded copy."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (loop for index from start below (or end (length string))
          do (application-async-lisp-output-stream--accept stream (char string index))))
  (application-async-lisp-output-stream--display stream nil)
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream application-async-lisp-output-stream))
  "Return STREAM's synchronized logical output column."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (call-next-method)))

(defmethod trivial-gray-streams:stream-force-output ((stream application-async-lisp-output-stream))
  "Offer pending output without bypassing the live display rate limit."
  (application-async-lisp-output-stream--display stream nil)
  nil)

(defmethod trivial-gray-streams:stream-finish-output ((stream application-async-lisp-output-stream))
  "Offer pending output; the job finalizer explicitly drains any throttled tail."
  (application-async-lisp-output-stream--display stream nil)
  nil)

(-> application-async-lisp-output-text (application-async-lisp-output-stream) string)
(defun application-async-lisp-output-text (stream)
  "Return bounded sanitized output with truncation and presentation failure details."
  (with-lock-held ((application-async-lisp-output-stream-lock stream))
    (let* ((capacity (bounded-output-stream-capacity stream))
           (marker
             (bounded-string
              (format nil "~A~@[~%[live output failed: ~A]~]"
                      (if (bounded-output-stream-truncated-p stream)
                          (format nil "~%[output truncated]")
                          "")
                      (application-async-lisp-output-stream-display-error stream))
              :limit capacity))
           (text     (bounded-output-stream-text stream)))
      (concatenate 'string
                   (take (- capacity (length marker)) text)
                   marker))))

(-> application-async-lisp-output-flush (application-async-lisp-output-stream) null)
(defun application-async-lisp-output-flush (stream)
  "Flush STREAM's pending display tail immediately."
  (application-async-lisp-output-stream--display stream t)
  nil)
