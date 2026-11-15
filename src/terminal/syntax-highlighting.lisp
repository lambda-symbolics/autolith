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

(-> syntax--lisp-parenthesis-roles (string) simple-vector)
(defun syntax--lisp-parenthesis-roles (source)
  "Return depth roles for structural parentheses without reading Lisp SOURCE.

Strings, escaped symbol names, character literals and nested comments do not
contribute to nesting. Incomplete input is accepted for diagnostic displays."
  (let ((roles          (make-array (length source) :initial-element nil))
        (depth          0)
        (index          0)
        (block-depth    0)
        (string-p       nil)
        (symbol-p       nil)
        (token-p        nil)
        (line-comment-p nil))
    (loop while (< index (length source))
          for character = (char source index)
          for next = (and (< (1+ index) (length source)) (char source (1+ index)))
          do (cond
               (line-comment-p
                (when (find character '(#\Newline #\Return))
                  (setf line-comment-p nil)))
               ((plusp block-depth)
                (cond
                  ((and (char= character #\#) (eql next #\|))
                   (incf block-depth)
                   (incf index))
                  ((and (char= character #\|) (eql next #\#))
                   (decf block-depth)
                   (incf index))))
               ((or string-p symbol-p)
                (cond
                  ((char= character #\\)
                   (incf index))
                  ((and string-p (char= character #\"))
                   (setf string-p nil))
                  ((and symbol-p (char= character #\|))
                   (setf symbol-p nil))))
               ((char= character #\;)
                (setf line-comment-p t token-p nil))
               ((find character '(#\Space #\Tab #\Newline #\Return #\Page #\' #\` #\,))
                (setf token-p nil))
               ((and (not token-p) (char= character #\#) (eql next #\|))
                (setf block-depth 1)
                (incf index))
               ((and (not token-p) (char= character #\#) (eql next #\\))
                (incf index 2)
                (when (and (< index (length source)) (alphanumericp (char source index)))
                  (loop while (and (< (1+ index) (length source))
                                   (not (find (char source (1+ index))
                                              '(#\Space #\Tab #\Newline #\Return #\Page
                                                #\( #\) #\" #\; #\' #\` #\,))))
                        do (incf index))))
               ((and (not token-p) (char= character #\#))
                ;; Consume a dispatch prefix, retaining its opening list delimiter.
                (loop while (and next (digit-char-p next))
                      do (incf index)
                         (setf next (and (< (1+ index) (length source))
                                         (char source (1+ index)))))
                (setf token-p (eql next #\:))
                (unless (eql next #\()
                  (incf index)))
               ((char= character #\\)
                (setf token-p t)
                (incf index))
               ((char= character #\")
                (setf string-p t token-p nil))
               ((char= character #\|)
                (setf symbol-p t token-p t))
               ((char= character #\()
                (setf token-p nil)
                (setf (aref roles index)
                      (nth (mod depth (length *terminal-parenthesis-style-names*))
                           *terminal-parenthesis-style-names*))
                (incf depth))
               ((char= character #\))
                (setf token-p nil)
                (setf depth (max 0 (1- depth))
                      (aref roles index)
                      (nth (mod depth (length *terminal-parenthesis-style-names*))
                           *terminal-parenthesis-style-names*)))
               (t
                (setf token-p t)))
             (incf index))
    roles))

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
         (roles  (syntax--lisp-parenthesis-roles source))
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
        (syntax--parenthesis-spans spans (syntax--lisp-parenthesis-roles source) 0)
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
