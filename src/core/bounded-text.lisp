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
          (if uri
              (format nil "~A~%... ~:D characters omitted; read the complete result at ~A"
                      (subseq text 0 limit)
                      (- (length text) limit)
                      uri)
              (format nil "~A~%... ~:D characters omitted"
                      (subseq text 0 limit)
                      (- (length text) limit)))))))
