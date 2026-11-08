(in-package #:autolith)

;;;; -- Bounded Presentation --

(-> bounded-string
    (t &key (:limit integer) (:overflow-uri-function (option function)))
    string)
(defun bounded-string (value &key (limit 8000) overflow-uri-function)
  "Render VALUE as a string no longer than LIMIT characters.

OVERFLOW-URI-FUNCTION, when supplied, receives the complete text of an
oversized VALUE and may return a URI where that text stays readable;
the truncation notice then tells the reader where to continue instead
of discarding the tail."
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
          (let ((kept (bounded-string-prefix-length (length text) :limit limit :uri uri)))
            (concatenate 'string
                         (subseq text 0 kept)
                         (bounded-string--notice (- (length text) kept) uri)))))))

(-> bounded-string-prefix-length (integer &key (:limit integer) (:uri (option string))) integer)
(defun bounded-string-prefix-length (length &key (limit 8000) uri)
  "Return how many leading characters of a LENGTH-character text `bounded-string' keeps within LIMIT.

A text that fits is kept whole. Otherwise the omission notice, naming URI
when there is one, takes its share of LIMIT; the widest count sizes it."
  (if (<= length limit)
      length
      (max 0 (- limit (length (bounded-string--notice length uri))))))

(-> bounded-string--notice (integer (option string)) string)
(defun bounded-string--notice (omitted uri)
  "Return the line telling the reader OMITTED characters were left out, and where to read them when URI says."
  (if uri
      (format nil "~%... ~:D characters omitted; read the complete result at ~A" omitted uri)
      (format nil "~%... ~:D characters omitted" omitted)))
