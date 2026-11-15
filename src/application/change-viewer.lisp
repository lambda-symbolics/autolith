(in-package #:autolith)

;;;; -- Shared Change Viewer --

(defparameter *application-tool-call-lines* 8
  "The maximum tool input lines shown in the terminal transcript.")

(defparameter *change-viewer-line-limit* 8
  "The default maximum removed or added lines shown by the shared viewer.")


;;; Text preparation

(-> application--display-lines (string) list)
(defun application--display-lines (text)
  "Return sanitized logical lines from TEXT without trailing blank rows."
  (let ((trimmed (string-right-trim '(#\Newline #\Return) text)))
    (when (plusp (length trimmed))
      (mapcar (lambda (line)
                (sanitize-text (string-right-trim '(#\Return) line)
                               :single-line-p t))
              (uiop:split-string trimmed :separator '(#\Newline))))))

;;; Source previews

(-> application--source-preview-rows
    (string &key (:path (option string)) (:language (option language))
                 (:prompt (option string)) (:limit (integer 1))
                 (:rainbow-parentheses-p boolean))
    list)
(defun application--source-preview-rows
    (text &key path language prompt (limit *application-tool-call-lines*)
               rainbow-parentheses-p)
  "Return bounded syntax-highlighted source TEXT under one green ruler.

PROMPT, when supplied, leads the first line and indents the following ones.
RAINBOW-PARENTHESES-P enables nesting colors for dedicated Lisp displays."
  (let ((rows (render-source text
                             :source-path            path
                             :syntax-language        language
                             :prompt                 prompt
                             :line-limit             limit
                             :sanitize-line-function #'application--sanitize-change-line
                             :span-function          #'syntax--terminal-span)))
    (if rainbow-parentheses-p
        (syntax--rainbow-source-rows rows)
        rows)))


;;; Before-and-after viewer

(-> application--sanitize-change-line (string) string)
(defun application--sanitize-change-line (line)
  "Return one terminal-safe display line for the shared change viewer."
  (sanitize-text line :single-line-p t))

(-> change-viewer-render
    (&key (:removed-content (option string))
          (:added-content (option string))
          (:removed-start-line (option integer))
          (:added-start-line (option integer))
          (:source-path (option string))
          (:syntax-language (option language))
          (:syntax-highlight-p boolean)
          (:line-limit (integer 1)))
    list)
(defun change-viewer-render
    (&key removed-content added-content
          removed-start-line added-start-line
          source-path syntax-language
          (syntax-highlight-p
            (not (null (or source-path syntax-language))))
          (line-limit *change-viewer-line-limit*))
  "Render one bounded line-numbered change with optional syntax highlighting.

REMOVED-CONTENT and ADDED-CONTENT are complete before-and-after documents for
one change. A NIL side denotes an absent document, so creation and removal use
the same renderer. SOURCE-PATH is language-classification metadata only and is
never read. SYNTAX-LANGUAGE overrides path inference, while
SYNTAX-HIGHLIGHT-P can explicitly disable highlighting. Exact line coordinates
remain optional rather than being fabricated."
  (render-diff
   :removed-content        removed-content
   :added-content          added-content
   :removed-start-line     removed-start-line
   :added-start-line       added-start-line
   :source-path            source-path
   :syntax-language        syntax-language
   :syntax-highlight-p     syntax-highlight-p
   :line-limit             line-limit
   :sanitize-line-function #'application--sanitize-change-line
   :span-function          #'syntax--terminal-span))
