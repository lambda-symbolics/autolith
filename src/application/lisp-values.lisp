(in-package #:autolith)

;;;; -- Human-Readable Lisp Values --

(defclass application-lisp-value-stream (bounded-output-stream)
  ((count
    :initform      0
    :accessor      application-lisp-value-stream--count
    :documentation "The number of emitted characters.")
   (overflow-tag
    :initarg       :overflow-tag
    :reader        application-lisp-value-stream--overflow-tag
    :documentation "The catch tag used to stop rendering when the buffer fills."))
  (:documentation "A bounded value stream that stops a printer at its output limit."))

(defmethod trivial-gray-streams:stream-write-char
    ((stream application-lisp-value-stream) character)
  "Stop printing rather than discard an unbounded amount of output."
  (when (>= (application-lisp-value-stream--count stream)
            (bounded-output-stream-capacity stream))
    (throw (application-lisp-value-stream--overflow-tag stream) t))
  (incf (application-lisp-value-stream--count stream))
  (call-next-method))

(defmethod trivial-gray-streams:stream-write-string
    ((stream application-lisp-value-stream) string &optional (start 0) end)
  "Write STRING through the same output limit as individual characters."
  (loop for index from start below (or end (length string))
        do (write-char (char string index) stream))
  string)

(-> application-lisp--render-value
    (t &key (:columns (integer 1)) (:initial-column (integer 0)))
    (values string boolean))
(defun application-lisp--render-value (value &key (columns 80) (initial-column 0))
  "Render VALUE for a human within COLUMNS available terminal columns.

Preserve reader syntax for representable values, including strings and shared
structure. Use the standard # and ... markers at depth 10 and length 100;
limit output to 200 lines and 16000 characters. An output-limit notice marks
an incomplete prefix. Printer failures produce a safe unreadable marker.
Return the printed text and whether printing succeeded. This display policy
does not change machine serialization or worker output."
  (let* ((tag                     (gensym "VALUE-OUTPUT-LIMIT"))
         (stream                  (make-instance 'application-lisp-value-stream
                                                 :builder      (bounded-output-stream--make-builder 16000)
                                                 :overflow-tag tag))
         (*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (*print-pretty*          t)
         (*print-circle*          t)
         (*print-escape*          t)
         (*print-gensym*          t)
         (*print-array*           t)
         (*print-readably*        nil)
         (*print-case*            ':upcase)
         (*print-base*            10)
         (*print-radix*           nil)
         (*print-level*           10)
         (*print-length*          100)
         (*print-lines*           200)
         (*print-right-margin*    (+ columns initial-column))
         (*print-miser-width*     nil))
    (setf (bounded-output-stream--column stream) initial-column)
    (set-pprint-dispatch '(satisfies application-lisp-value--plist-p)
                         #'application-lisp-value--pprint-plist
                         1)
    (handler-case
        (let ((truncated-p
                (catch tag
                  (write value :stream stream)
                  nil)))
          (values (if truncated-p
                      (concatenate 'string (bounded-output-stream-text stream)
                                   (format nil "~%... [output truncated]"))
                      (bounded-output-stream-text stream))
                  t))
      (error ()
        (values "#<printing failed>" nil)))))

(-> application-lisp-value--plist-p (t) boolean)
(defun application-lisp-value--plist-p (value)
  "Recognize nonempty proper keyword plists with at most 100 pairs.

Bound the scan so circular, dotted, and long lists use the standard printer."
  (and (consp value)
       (plist-schema-p value :keyword-keys-p t :maximum-length 200 :allow-duplicate-keys t)))

(-> application-lisp-value--pprint-plist (stream list) null)
(defun application-lisp-value--pprint-plist (stream value)
  "Print VALUE with aligned keyword/value rows when it does not fit one line."
  (let ((width
          (loop for (key) on value by (partial #'nthcdr 2)
                maximize (min 24 (length (write-to-string key :pretty nil))))))
    (pprint-logical-block (stream value :prefix "(" :suffix ")")
      (loop
        (pprint-exit-if-list-exhausted)
        (let ((key (pprint-pop)))
          (write key :stream stream)
          (write-string (make-string (max 1 (- (1+ width)
                                               (length (write-to-string key :pretty nil))))
                                     :initial-element #\Space)
                        stream))
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)
        (pprint-newline ':linear stream))))
  nil)

(-> application-lisp-render-value (t &key (:columns (integer 1))) string)
(defun application-lisp-render-value (value &key (columns 80))
  "Render VALUE as bounded, readable Lisp within COLUMNS available columns.

Keep reader syntax for strings, symbols, and shared or circular structures.
Limit depth to 10, sequence length to 100, lines to 200, and characters to
16000. Use standard printer omission markers and an explicit notice when
character output stops early. A failed object printer produces a safe marker."
  (values (application-lisp--render-value value :columns columns)))
