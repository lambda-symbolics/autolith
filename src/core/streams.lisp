(in-package #:autolith)

;;;; -- Bounded UTF-8 Files --

(-> file--read-bounded-utf-8
    (pathname
     &key (:maximum-bytes (integer 0))
          (:tool-name non-empty-string)
          (:description non-empty-string)
          (:validation-function (option function)))
    string)
(defun file--read-bounded-utf-8
    (path &key maximum-bytes tool-name description validation-function)
  "Read a bounded regular PATH file as stable UTF-8 text.

ls-compat opens PATH without following links, enforces MAXIMUM-BYTES, runs
VALIDATION-FUNCTION after opening, and confirms that PATH still names the
opened, unchanged object. Each of its conditions becomes a TOOL-ERROR naming
TOOL-NAME and DESCRIPTION."
  (let ((native-path (uiop:native-namestring path)))
    (flet ((fail (control &rest arguments)
             (error 'tool-error
                    :message   (apply #'format nil control arguments)
                    :tool-name tool-name)))
      (handler-case
          (read-file-text path
                          :maximum-octets      maximum-bytes
                          :validation-function validation-function)
        (file-too-large (condition)
          (fail "~A ~A is ~:D bytes; ~A reads exact UTF-8 files only up to ~:D bytes."
                description
                native-path
                (file-too-large-size condition)
                tool-name
                maximum-bytes))
        (file-changed ()
          (fail "~A ~A changed while it was being read. Reread it and retry."
                description native-path))
        (file-not-utf-8 ()
          (fail "~A ~A is not valid UTF-8 text." description native-path))
        (not-regular-file (condition)
          (if (eq (not-regular-file-kind condition) ':symbolic-link)
              (fail "Could not read ~A ~A as an exact regular file: ~A"
                    description native-path condition)
              (fail "~A ~A is not a regular file." description native-path)))
        (file-error (condition)
          (error 'tool-error
                 :message   (format nil "Could not read ~A ~A as an exact regular file: ~A"
                                    description native-path condition)
                 :tool-name tool-name
                 :code      (tool-failure-code condition)))))))


;;;; -- Bounded Text Windows --

(-> text--split-lines (string) vector)
(defun text--split-lines (content)
  "Return CONTENT's logical lines without their LF or CRLF delimiters."
  (coerce (serapeum:lines content :eol-style ':lf :honor-crlf t) 'vector))

(-> text--numbered-line-window
    (vector &key (:start-line (integer 1))
                 (:line-count (integer 1))
                 (:maximum-characters (integer 1))
                 (:line-anchor-function (option function)))
    (values string list (integer 0) boolean))
(defun text--numbered-line-window
    (lines &key start-line line-count maximum-characters line-anchor-function)
  "Render a bounded numbered LINES window and return its visible range and status.

When LINE-ANCHOR-FUNCTION is supplied, render its short annotation beside each
line number."
  (let* ((total-lines    (length lines))
         (last-requested (min total-lines (+ start-line line-count -1)))
         (body           (make-array maximum-characters
                                     :element-type 'character
                                     :fill-pointer 0))
         (visible-start  nil)
         (visible-end    nil)
         (truncated-p    nil))
    (loop for line from start-line to last-requested
          for text = (aref lines (1- line))
          for anchor = (and line-anchor-function
                            (funcall line-anchor-function text))
          for rendered = (if anchor
                             (format nil "~6D:~A  ~A" line anchor text)
                             (format nil "~6D  ~A" line text))
          for separator-length = (if visible-start 1 0)
          do
             (if (> (+ (fill-pointer body)
                       separator-length
                       (length rendered))
                    (array-total-size body))
                 (progn
                   (setf truncated-p t)
                   (return))
                 (progn
                   (when visible-start
                     (vector-push #\Newline body))
                   (loop for character across rendered
                         do (vector-push character body))
                   (unless visible-start
                     (setf visible-start line))
                   (setf visible-end line))))
    (values (coerce body 'string)
            (if visible-start (list (list visible-start visible-end)) nil)
            (or visible-end 0)
            truncated-p)))


;;;; -- Bounded Output Capture --

(defclass bounded-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((builder
    :initarg       :builder
    :reader        bounded-output-stream--builder
    :documentation "The structlisp builder retaining the captured prefix.")
   (column
    :initform      0
    :accessor      bounded-output-stream--column
    :documentation "The logical output column for FORMAT and FRESH-LINE."))
  (:documentation
   "A character output stream retaining at most a fixed prefix of its output.

Output beyond the capacity is discarded and marks the stream truncated, while
the logical column keeps following everything written."))

(-> bounded-output-stream-create ((integer 0)) bounded-output-stream)
(defun bounded-output-stream-create (capacity)
  "Return a stream retaining at most CAPACITY characters."
  (make-instance 'bounded-output-stream
                 :builder (bounded-output-stream--make-builder capacity)))

(-> bounded-output-stream-text (bounded-output-stream) string)
(defun bounded-output-stream-text (stream)
  "Return a fresh string holding STREAM's retained prefix."
  (coerce (bounded-sequence-builder-snapshot (bounded-output-stream--builder stream))
          'string))

(-> bounded-output-stream-truncated-p (bounded-output-stream) boolean)
(defun bounded-output-stream-truncated-p (stream)
  "Return true when STREAM discarded output beyond its capacity."
  (bounded-sequence-builder-overflowed-p (bounded-output-stream--builder stream)))

(-> bounded-output-stream-capacity (bounded-output-stream) (integer 0))
(defun bounded-output-stream-capacity (stream)
  "Return the most characters STREAM retains."
  (bounded-sequence-builder-maximum-count (bounded-output-stream--builder stream)))

(-> bounded-output-stream--make-builder ((integer 0)) bounded-sequence-builder)
(defun bounded-output-stream--make-builder (capacity)
  "Return an empty character builder bounded to CAPACITY."
  (make-bounded-sequence-builder capacity :element-type 'character))

(-> bounded-output-stream--capture (bounded-output-stream character) null)
(defun bounded-output-stream--capture (stream character)
  "Retain CHARACTER while STREAM has room and advance its logical column."
  (bounded-sequence-builder-try-append (bounded-output-stream--builder stream) character)
  (setf (bounded-output-stream--column stream)
        (case character
          ((#\Newline #\Return)
           0)
          (#\Tab
           (let ((column (bounded-output-stream--column stream)))
             (+ column (- 8 (mod column 8)))))
          (otherwise
           (1+ (bounded-output-stream--column stream)))))
  nil)

(defmethod trivial-gray-streams:stream-write-char ((stream bounded-output-stream) character)
  "Capture CHARACTER within STREAM's capacity."
  (bounded-output-stream--capture stream character)
  character)

(defmethod trivial-gray-streams:stream-write-string
    ((stream bounded-output-stream) string &optional (start 0) end)
  "Capture the range of STRING that fits STREAM's capacity."
  (loop for index from start below (or end (length string))
        do (bounded-output-stream--capture stream (char string index)))
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream bounded-output-stream))
  "Return STREAM's logical output column."
  (bounded-output-stream--column stream))
