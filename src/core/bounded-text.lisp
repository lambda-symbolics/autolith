(in-package #:autolith)

;;;; -- Bounded Presentation --

(-> bounded-string
    (t &key (:limit integer) (:overflow-uri-function (option function)))
    string)
(defun bounded-string (value &key (limit 8000) overflow-uri-function)
  "Render VALUE, retaining at most LIMIT characters before the truncation notice.

OVERFLOW-URI-FUNCTION receives the complete text when truncation is needed.
If it returns a URI, include that URI in the notice."
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
