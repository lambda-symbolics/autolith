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

(-> syntax--highlight-spans
    (string &key (:language (option language)) (:pathname t)
                 (:rainbow-parentheses-p boolean))
    (option list))
(defun syntax--highlight-spans (source &key language pathname rainbow-parentheses-p)
  "Return syntax-highlighted SOURCE, optionally with Lisp nesting colors."
  (highlight-spans source
                   :language              language
                   :pathname              pathname
                   :role-overlay-function (when rainbow-parentheses-p
                                            #'syntax--parenthesis-roles)
                   :span-function         #'syntax--terminal-span))
