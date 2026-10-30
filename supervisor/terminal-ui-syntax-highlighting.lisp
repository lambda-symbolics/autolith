(in-package #:autolith-supervisor)

;;;; -- Semantic Syntax Highlighting --

(serapeum/types:-> syntax--terminal-span (keyword string) cons)
(defun syntax--terminal-span (role text)
  "Return one Autolith terminal span for Colordiff ROLE and TEXT."
  (terminal-span
   (case role
     (:context-gutter
      ':dim)
     (:removed-gutter
      ':failure)
     ((:added-gutter :source-gutter)
      ':success)
     ((:context :elision :prompt)
      ':dim)
     (otherwise
      role))
   text))

(serapeum/types:-> syntax--highlight-spans
    (string &key (:language (option colorlisp:language)) (:pathname t))
    (option list))
(defun syntax--highlight-spans (source &key colorlisp:language pathname)
  "Return SOURCE as one sequence of syntax-highlighted terminal spans."
  (colordiff:highlight-spans source
                   :language      colorlisp:language
                   :pathname      pathname
                   :span-function #'syntax--terminal-span))

(serapeum/types:-> syntax--highlight-lines
    (string &key (:language (option colorlisp:language)) (:pathname t))
    (option vector))
(defun syntax--highlight-lines (source &key colorlisp:language pathname)
  "Return SOURCE as syntax-highlighted rows, or NIL without a language."
  (colordiff:highlight-lines source
                   :language      colorlisp:language
                   :pathname      pathname
                   :span-function #'syntax--terminal-span))
