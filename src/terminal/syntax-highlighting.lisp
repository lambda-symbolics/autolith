(in-package #:autolith)

;;;; -- Semantic Syntax Highlighting --

(defparameter *application-common-lisp-language*
  (language-find "common-lisp" :errorp nil)
  "The ColorLisp language used for pathless Common Lisp source previews.")

(defparameter *application-shell-language*
  (language-find "bash" :errorp nil)
  "The ColorLisp language used for shell command previews.")

(-> syntax--terminal-span (keyword string) cons)
(defun syntax--terminal-span (role text)
  "Return one Autolith terminal span for Colordiff ROLE and TEXT."
  (terminal-span
   (case role
     (:removed-gutter
      ':failure)
     (:added-gutter
      ':success)
     ((:context-gutter :source-gutter :context :elision :prompt)
      ':dim)
     (otherwise
      role))
   text))

(-> syntax--parenthesis-roles (string) simple-vector)
(defun syntax--parenthesis-roles (source)
  "Map structural parenthesis depths in SOURCE to the terminal palette."
  (let ((palette-size (length *terminal-parenthesis-style-names*)))
    (map 'simple-vector
         (lambda (depth)
           (when depth
             (nth (mod depth palette-size) *terminal-parenthesis-style-names*)))
         (common-lisp-delimiter-depths source))))

(-> syntax--parenthesis-spans (list simple-vector (integer 0)) list)
(defun syntax--parenthesis-spans (spans roles offset)
  "Split SPANS at parenthesis roles, starting at their SOURCE character OFFSET."
  (loop for span in spans
        for text = (terminal-span-text span)
        append (loop with start = 0
                     while (< start (length text))
                     for role = (or (aref roles (+ offset start)) (terminal-span-style span))
                     for end = (loop for index from (1+ start) below (length text)
                                     unless (eq role (or (aref roles (+ offset index))
                                                         (terminal-span-style span)))
                                       return index
                                     finally (return (length text)))
                     collect (terminal-span role (subseq text start end))
                     do (setf start end))
        do (incf offset (length text))))

(-> syntax--rainbow-source-rows (list) list)
(defun syntax--rainbow-source-rows (rows)
  "Apply nesting colors to source ROWS, preserving their leading gutter spans."
  (let* ((source (format nil "~{~A~^~%~}"
                         (loop for row in rows collect (terminal--spans-text (rest row)))))
         (roles  (syntax--parenthesis-roles source))
         (offset 0))
    (loop for row in rows
          collect (cons (first row) (syntax--parenthesis-spans (rest row) roles offset))
          do (incf offset (1+ (length (terminal--spans-text (rest row))))))))

(-> syntax--highlight-spans
    (string &key (:language (option language)) (:pathname t)
                 (:rainbow-parentheses-p boolean))
    (option list))
(defun syntax--highlight-spans (source &key language pathname rainbow-parentheses-p)
  "Return syntax-highlighted SOURCE, optionally with Lisp nesting colors."
  (let ((spans (highlight-spans source
                               :language      language
                               :pathname      pathname
                               :span-function #'syntax--terminal-span)))
    (if (and spans rainbow-parentheses-p)
        (syntax--parenthesis-spans spans (syntax--parenthesis-roles source) 0)
        spans)))

(-> syntax--highlight-lines
    (string &key (:language (option language)) (:pathname t))
    (option vector))
(defun syntax--highlight-lines (source &key language pathname)
  "Return SOURCE as syntax-highlighted rows, or NIL without a language."
  (highlight-lines source
                   :language      language
                   :pathname      pathname
                   :span-function #'syntax--terminal-span))
