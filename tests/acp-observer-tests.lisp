(in-package #:autolith)

;;;; -- ACP Tool Presentation Tests --

(-> test-acp-observer-tool-title-previews () null)
(defun test-acp-observer-tool-title-previews ()
  "Preview principal arguments without control characters or unbounded titles."
  (dolist (case (list (list "lisp.eval" (json-object "compile" t "forms" #("(load-it)" "(+ 1 2)")) "(load-it) (+ 1 2)")
                     (list "shell.run" (json-object "async" t "command" "git status") "git status")
                     (list "resource.read" (json-object "uri" "conversation:current") "conversation:current")
                     (list "custom" (json-object "z" 9 "a" #(1 2)) "2 items")
                     (list "custom" (json-object "flag" (argo:json-false)) "false")))
    (destructuring-bind (name arguments preview) case
      (test-assert (search preview (acp-tool-title name arguments))
                   "the title previews the principal argument")))
  (dolist (arguments (list nil (json-object) (json-object "form" "")))
    (test-assert (string= "lisp.eval" (acp-tool-title "lisp.eval" arguments))
                 "empty arguments use the canonical tool name"))
  (let ((title (acp-tool-title "lisp.eval"
                               (json-object "form" (format nil "~C[31m~A~%tail"
                                                            #\Esc (make-string 100 :initial-element #\界))))))
    (test-assert (<= (text-cell-width title) (+ (text-cell-width "lisp.eval") 3 48))
                 "argument previews are bounded in display cells")
    (test-assert (not (find-if (lambda (character)
                                (or (< (char-code character) 32)
                                    (= (char-code character) 127)))
                              title))
                 "titles contain no control characters"))
  (test-call-with-function-replacements
   (list (list 'json-encode (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (error "Compound title previews must not serialize their payload.")))
         (list 'sanitize-text (lambda (text &key single-line-p)
                               (declare (ignore single-line-p))
                               (test-assert (<= (length text) 192)
                                            "title sanitization consumes a bounded prefix")
                               text)))
   (lambda ()
     (acp-tool-title "lisp.eval" (json-object "form" (make-string 1000000 :initial-element #\x)))
     (acp-tool-title "custom" (json-object "items" (make-array 100000 :initial-element "large")))
     (acp-tool-title "lisp.eval" (json-object "forms" (make-array 4 :initial-element (make-string 1000000 :initial-element #\x))))))
  nil)
