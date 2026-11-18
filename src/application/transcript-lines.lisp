(in-package #:autolith)

;;;; -- Transcript Lines --

;;; Autolith's own words in the transcript are colored lines that begin with
;;; a marker and a lowercase letter. The marker tells them apart: a dot for a
;;; notice or a hint, an exclamation point for a failure, a check mark for
;;; finished work.

(-> application--line-text (string) string)
(defun application--line-text (text)
  "Return TEXT as a transcript line reads.

An ordinary capitalized first word starts lowercase, one final period is
dropped, and later lines are indented beneath the marker."
  (let* ((sentence-p (and (> (length text) 1)
                          (upper-case-p (char text 0))
                          (or (lower-case-p (char text 1))
                              (char= (char text 1) #\Space))))
         (end        (if (and (> (length text) 1)
                              (char= (char text (1- (length text))) #\.)
                              (char/= (char text (- (length text) 2)) #\.))
                         (1- (length text))
                         (length text)))
         (line       (subseq text 0 end)))
    (when sentence-p
      (setf (char line 0) (char-downcase (char line 0))))
    (with-output-to-string (stream)
      (loop for character across line
            do (write-char character stream)
               (when (char= character #\Newline)
                 (write-string "  " stream))))))

(-> application--marked-line (terminal-style string string) list)
(defun application--marked-line (style marker text)
  "Return TEXT as one STYLE line that begins with MARKER."
  (list (terminal-span style (format nil "~A ~A" marker (application--line-text text)))))

(-> application-notice-line (string) list)
(defun application-notice-line (text)
  "Return TEXT as a notice line, marked with a dot."
  (application--marked-line ':notice "·" text))

(-> application-hint-line (string) list)
(defun application-hint-line (text)
  "Return TEXT as a quiet line for an empty state or a next step, marked with a dot."
  (application--marked-line ':hint "·" text))

(-> application-success-line (string) list)
(defun application-success-line (text)
  "Return TEXT as a receipt for finished work, marked with a check mark."
  (application--marked-line ':success "✓" text))

(-> application-failure-line (string) list)
(defun application-failure-line (text)
  "Return TEXT as a failure line, marked with an exclamation point."
  (application--marked-line ':failure "!" text))
