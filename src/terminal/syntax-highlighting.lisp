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

(-> syntax--highlight-spans
    (string &key (:language (option language)) (:pathname t))
    (option list))
(defun syntax--highlight-spans (source &key language pathname)
  "Return SOURCE as one sequence of syntax-highlighted terminal spans."
  (highlight-spans source
                   :language      language
                   :pathname      pathname
                   :span-function #'syntax--terminal-span))

(-> syntax--highlight-lines
    (string &key (:language (option language)) (:pathname t))
    (option vector))
(defun syntax--highlight-lines (source &key language pathname)
  "Return SOURCE as syntax-highlighted rows, or NIL without a language."
  (highlight-lines source
                   :language      language
                   :pathname      pathname
                   :span-function #'syntax--terminal-span))
