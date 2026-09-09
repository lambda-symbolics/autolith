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

VALIDATION-FUNCTION runs after opening the file and before confirming that
PATH still names the opened object."
  (let ((native-path (uiop:native-namestring path)))
    (labels ((fail (control &rest arguments)
               (error 'tool-error
                      :message (apply #'format nil control arguments)
                      :tool-name tool-name))

             (changed ()
               (fail "~A ~A changed while it was being read. Reread it and retry."
                     description native-path)))
      (let ((stream nil))
        (unwind-protect
             (handler-case
                 (multiple-value-bind (opened status)
                     (platform-open-regular-file *platform* path)
                   (setf stream opened)
                   (when (> (platform-file-status-size status) maximum-bytes)
                     (fail "~A ~A is ~:D bytes; ~A reads exact UTF-8 files only up to ~:D bytes."
                           description
                           native-path
                           (platform-file-status-size status)
                           tool-name
                           maximum-bytes))
                   (when validation-function
                     (funcall validation-function))
                   (let ((current (platform-path-status *platform* path
                                                        :follow-links-p t)))
                     (unless (and current
                                  (platform-file-status-same-object-p status
                                                                      current))
                       (changed)))
                   (let* ((length (platform-file-status-size status))
                          (octets (make-array length
                                              :element-type '(unsigned-byte 8))))
                     (unless (= (read-sequence octets stream) length)
                       (changed))
                     (unless (platform-file-status-unchanged-p
                              status
                              (platform-stream-status *platform* stream))
                       (changed))
                     (handler-case
                         (sb-ext:octets-to-string octets :external-format ':utf-8)
                       (error ()
                         (fail "~A ~A is not valid UTF-8 text."
                               description native-path)))))
               (platform-error (condition)
                 (if (eq (platform-error-reason condition) ':not-regular)
                     (fail "~A ~A is not a regular file."
                           description native-path)
                     (fail "Could not read ~A ~A as an exact regular file: ~A"
                           description native-path condition))))
          (when stream
            (close stream)))))))


;;;; -- Bounded Text Windows --

(-> text--split-lines (string) vector)
(defun text--split-lines (content)
  "Return CONTENT's logical lines without their LF or CRLF delimiters."
  (if (zerop (length content))
      #()
      (let ((lines nil)
            (start 0)
            (length (length content)))
        (loop
          for newline = (position #\Newline content :start start)
          for end = (or newline length)
          for logical-end = (if (and newline
                                     (> end start)
                                     (char= (char content (1- end)) #\Return))
                                (1- end)
                                end)
          do (push (subseq content start logical-end) lines)
          if newline
            do (setf start (1+ newline))
          else
            do (return)
          when (= start length)
            do (return))
        (coerce (nreverse lines) 'vector))))

(-> text--numbered-line-window
    (vector (integer 1) (integer 1) (integer 1)
     &key (:line-anchor-function (option function)))
    (values string list (integer 0) boolean))
(defun text--numbered-line-window
    (lines start-line line-count maximum-characters &key line-anchor-function)
  "Render a bounded numbered LINES window and return its visible range and status.

When LINE-ANCHOR-FUNCTION is supplied, render its short annotation beside each
line number."
  (let* ((total-lines (length lines))
         (last-requested (min total-lines (+ start-line line-count -1)))
         (body (make-array maximum-characters
                           :element-type 'character
                           :fill-pointer 0))
         (visible-start nil)
         (visible-end nil)
         (truncated-p nil))
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
