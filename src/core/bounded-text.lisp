(in-package #:autolith)

;;;; -- Bounded Presentation --

(-> bounded-string
    (t &key (:limit integer) (:overflow-uri-function (option function)))
    string)
(defun bounded-string (value &key (limit 8000) overflow-uri-function)
  "Render VALUE as a string no longer than LIMIT characters.

OVERFLOW-URI-FUNCTION, when supplied, receives the complete text of an
oversized VALUE and may return a URI where that text is readable. Include
an omission notice with that URI when it fits; otherwise keep a bounded
prefix without a notice."
  (check-type limit (integer 0))
  (let ((text (if (stringp value)
                  value
                  (write-to-string value
                                   :circle   t
                                   :level    8
                                   :length   80
                                   :readably nil))))
    (if (<= (length text) limit)
        text
        (let ((uri (and overflow-uri-function
                        (funcall overflow-uri-function text))))
          (multiple-value-bind (kept notice)
              (bounded-string-prefix-length (length text) :limit limit :uri uri)
            (concatenate 'string (subseq text 0 kept) notice))))))

(-> bounded-string-prefix-length
    (integer &key (:limit integer) (:uri (option string)))
    (values integer (option string)))
(defun bounded-string-prefix-length (length &key (limit 8000) uri)
  "Return the retained prefix length and optional omission notice within LIMIT.

Keep text of LENGTH characters whole when it fits. Otherwise reserve room
for the widest omission count and URI. If the notice cannot fit, return
LIMIT and NIL instead."
  (check-type length (integer 0))
  (check-type limit (integer 0))
  (if (<= length limit)
      (values length nil)
      (let ((kept (- limit (length (bounded-string--notice length uri)))))
        (if (minusp kept)
            (values limit nil)
            (values kept (bounded-string--notice (- length kept) uri))))))

(-> bounded-string--notice (integer (option string)) string)
(defun bounded-string--notice (omitted uri)
  "Return the line telling the reader OMITTED characters were left out, and where to read them when URI says."
  (if uri
      (format nil "~%... ~:D characters omitted; read the complete result at ~A" omitted uri)
      (format nil "~%... ~:D characters omitted" omitted)))
