(in-package #:autolith)

;;;; -- Authored Onboarding Sources --

(defparameter *onboarding-source-snapshot* nil
  "The authored source snapshot dynamically bound for one tour, or NIL between tours.")

(define-condition onboarding-source-error (error)
  ((detail :initarg :detail :reader onboarding-source-error-detail
           :documentation "The invalid source construct."))
  (:documentation "An onboarding source cannot be decoded into lesson pages.")
  (:report (lambda (condition stream)
             (format stream "Invalid onboarding source: ~A"
                     (onboarding-source-error-detail condition)))))

(-> onboarding-source--fail (string) nil)
(defun onboarding-source--fail (detail)
  "Signal a source error describing DETAIL."
  (error 'onboarding-source-error :detail detail))

(-> onboarding-source--inline (string) list)
(defun onboarding-source--inline (text)
  "Decode strong emphasis, verbatim code, links, and semantic spans.

Recognize emphasis at Org word boundaries. Preserve literal punctuation and
newlines; decode links within strong text without treating code as markup."
  (let ((cursor      0)
        (plain-start 0)
        (fragments   nil)
        (length      (length text)))
    (labels ((at-p (prefix)
               (and (<= (+ cursor (length prefix)) length)
                    (string= prefix text :start2 cursor
                                         :end2   (+ cursor (length prefix)))))

             (flush-plain ()
               (when (< plain-start cursor)
                 (push (subseq text plain-start cursor) fragments)))

             (close-at (delimiter start)
               (or (search delimiter text :start2 start)
                   (onboarding-source--fail
                    (format nil "Unclosed ~A in ~S" delimiter text))))

             (markup-start-p ()
               (and (< (1+ cursor) length)
                    (not (find (char text (1+ cursor)) '(#\Space #\Tab #\Newline)))
                    (or (zerop cursor)
                        (find (char text (1- cursor))
                              '(#\Space #\Tab #\Newline #\- #\( #\" #\' #\{)))))

             (markup-end (delimiter)
               (or (loop for end = (position delimiter text :start (1+ cursor))
                           then (position delimiter text :start (1+ end))
                         while end
                         when (and (> end (1+ cursor))
                                   (not (find (char text (1- end)) '(#\Space #\Tab #\Newline)))
                                   (or (= (1+ end) length)
                                       (find (char text (1+ end))
                                             '(#\Space #\Tab #\Newline #\- #\. #\, #\: #\!
                                               #\? #\; #\' #\" #\) #\} #\[ #\] #\\))))
                           return end)
                   (onboarding-source--fail
                    (format nil "Unclosed ~A in ~S" delimiter text))))

             (emit (fragment end)
               (flush-plain)
               (push fragment fragments)
               (setf cursor end plain-start end)))
      (loop while (< cursor length)
            do (cond
                 ((at-p "[[")
                  (let* ((label (close-at "][" (+ cursor 2)))
                         (end   (close-at "]]" (+ label 2)))
                         (url   (subseq text (+ cursor 2) label))
                         (parts (onboarding-source--inline
                                 (subseq text (+ label 2) end))))
                    (flush-plain)
                    (dolist (part parts)
                      (when (and (listp part) (third part))
                        (onboarding-source--fail "Nested links are not supported."))
                      (push (if (stringp part)
                                (list ':strong part url)
                                (append part (list url)))
                            fragments))
                    (setf cursor (+ end 2) plain-start cursor)))
                 ((at-p "@@")
                  (let* ((colon (or (position #\: text :start (+ cursor 2))
                                    (onboarding-source--fail "Semantic span needs a style.")))
                         (name  (subseq text (+ cursor 2) colon))
                         (style (rest (assoc name '(("brand" . :brand) ("dim" . :dim)
                                                    ("lisp-prompt" . :lisp-prompt)
                                                    ("code" . :code) ("success" . :success)
                                                    ("lambda-logo-1" . :lambda-logo-1)
                                                    ("lambda-logo-2" . :lambda-logo-2)
                                                    ("lambda-logo-3" . :lambda-logo-3)
                                                    ("lambda-logo-4" . :lambda-logo-4)
                                                    ("lambda-logo-5" . :lambda-logo-5)
                                                    ("lambda-logo-6" . :lambda-logo-6))
                                             :test #'string=)))
                         (end   (close-at "@@" (1+ colon))))
                    (unless style
                      (onboarding-source--fail (format nil "Unknown style ~S" name)))
                    (emit (list style (subseq text (1+ colon) end)) (+ end 2))))
                 ((and (or (at-p "=") (at-p "*")) (markup-start-p))
                  (let* ((code-p   (at-p "="))
                         (end      (markup-end (char text cursor)))
                         (contents (subseq text (1+ cursor) end)))
                    (if code-p
                        (emit (list ':code contents) (1+ end))
                        (progn
                          (flush-plain)
                          (dolist (part (onboarding-source--inline contents))
                            (push (if (stringp part) (list ':strong part) part) fragments))
                          (setf cursor (1+ end) plain-start cursor)))))
                 (t
                  (incf cursor))))
      (flush-plain))
    (nreverse fragments)))

(-> onboarding-source--page (&key (:title string) (:properties list)
                                 (:lines list) (:variables hash-table)) list)
(defun onboarding-source--page (&key title properties lines variables)
  "Validate lesson metadata and render its authored lines with VARIABLES."
  (let* ((id            (rest (assoc "ID" properties :test #'string=)))
         (version-text  (rest (assoc "VERSION" properties :test #'string=)))
         (version       (and version-text
                             (handler-case (parse-integer version-text)
                               (parse-error ()
                                 nil))))
         (exercise-text (rest (assoc "EXERCISE" properties :test #'string=)))
         (exercise      (rest (assoc exercise-text
                                     '(("continue" . :continue) ("help" . :help)
                                       ("async" . :async) ("settings" . :settings)
                                       ("authentication" . :authentication))
                                     :test #'equal)))
         (body          (format nil "~{~A~^~%~}" (reverse lines))))
    (unless (and (non-empty-string-p title) (non-empty-string-p id)
                 (every (lambda (character)
                          (or (find character "abcdefghijklmnopqrstuvwxyz0123456789")
                              (char= character #\-)))
                        id)
                 (integerp version) (plusp version)
                 (or (null exercise-text) exercise))
      (onboarding-source--fail (format nil "Invalid metadata for ~S" title)))
    (list :id id :version version
          :title    (org-templater:render :template title :config variables)
          :exercise exercise
          :body
          (mapcar (lambda (fragment)
                    (if (stringp fragment)
                        (terminal-span ':plain
                                       (org-templater:render :template fragment :config variables))
                        (destructuring-bind (style text &optional url) fragment
                          (let ((rendered (org-templater:render :template text :config variables)))
                            (if url
                                (termdown:make-widget
                                 style rendered
                                 (list ':open-url
                                       (org-templater:render :template url :config variables)))
                                (terminal-span style rendered))))))
                  (onboarding-source--inline body)))))

(-> onboarding-source-pages (string hash-table) list)
(defun onboarding-source-pages (source variables)
  "Read ordered top-level Org lessons and render spans with VARIABLES.

Only the introductory comment, title directive, property drawers and lesson
text are structural. Keep body newlines verbatim apart from the one blank
line separating lessons. Reject duplicate IDs and malformed metadata."
  (let ((pages         nil)
        (title         nil)
        (properties    nil)
        (lines         nil)
        (drawer-p      nil)
        (drawer-seen-p nil)
        (comment-p     nil))
    (labels ((finish-page ()
               (when title
                 (when (or drawer-p (not drawer-seen-p))
                   (onboarding-source--fail "Missing or unclosed property drawer."))
                 (when (and lines (string= (first lines) ""))
                   (pop lines))
                 (let ((page (onboarding-source--page :title title :properties properties
                                                     :lines lines :variables variables)))
                   (when (find (getf page :id) pages :key (lambda (page) (getf page :id))
                                                    :test #'string=)
                     (onboarding-source--fail "Duplicate lesson ID."))
                   (push page pages)))))
      (with-input-from-string (input source)
        (loop for line = (read-line input nil nil)
              while line
              do (cond
                   ((string-equal line "#+end_comment")
                    (unless comment-p
                      (onboarding-source--fail "Unexpected comment end."))
                    (setf comment-p nil))
                   (comment-p)
                   ((and (not title) (string-equal line "#+begin_comment"))
                    (setf comment-p t))
                   ((uiop:string-prefix-p "* " line)
                    (finish-page)
                    (setf title (subseq line 2) properties nil lines nil
                          drawer-p nil drawer-seen-p nil))
                   ((and title (string= line ":PROPERTIES:") (not lines))
                    (when drawer-seen-p
                      (onboarding-source--fail "Duplicate property drawer."))
                    (setf drawer-p t drawer-seen-p t))
                   ((and drawer-p (string= line ":END:"))
                    (setf drawer-p nil))
                   (drawer-p
                    (let ((colon (and (plusp (length line))
                                      (char= (char line 0) #\:)
                                      (position #\: line :start 1))))
                      (unless colon
                        (onboarding-source--fail "Malformed property."))
                      (let ((name (subseq line 1 colon)))
                        (unless (and (member name '("ID" "VERSION" "EXERCISE") :test #'string=)
                                     (not (assoc name properties :test #'string=)))
                          (onboarding-source--fail "Unknown or duplicate property."))
                        (push (cons name (string-trim '(#\Space #\Tab)
                                                     (subseq line (1+ colon))))
                              properties))))
                   (title
                    (push line lines))
                   ((or (string= line "") (uiop:string-prefix-p "#+title:" line)))
                   (t
                    (onboarding-source--fail "Text outside a lesson.")))))
      (when comment-p
        (onboarding-source--fail "Unclosed introductory comment."))
      (finish-page))
    (unless pages
      (onboarding-source--fail "No lessons."))
    (nreverse pages)))

(-> onboarding-source--read () string)
(defun onboarding-source--read ()
  "Return the current tour snapshot or read the shipped ASDF-relative Org asset."
  (or *onboarding-source-snapshot*
      (uiop:read-file-string
       (asdf:system-relative-pathname :autolith-agent #p"docs/onboarding-sources.org"))))

(-> onboarding-source-text (string &optional hash-table) string)
(defun onboarding-source-text (name &optional (variables (make-hash-table :test #'eq)))
  "Render the named onboarding text block with VARIABLES on demand.

Place #+name: NAME immediately above #+begin_onboarding_text inside the
introductory comment. End with #+end_onboarding_text. Lesson parsing ignores
these UI templates; their placeholders are expanded only by this function."
  (let ((named-p   nil)
        (collect-p nil)
        (lines     nil))
    (with-input-from-string (input (onboarding-source--read))
      (loop for line = (read-line input nil nil)
            while line
            do (cond
                 ((and collect-p (string-equal line "#+end_onboarding_text"))
                  (return-from onboarding-source-text
                    (org-templater:render :template (format nil "~{~A~^~%~}" (nreverse lines))
                                         :config    variables)))
                 (collect-p
                  (push line lines))
                 ((and named-p (string-equal line "#+begin_onboarding_text"))
                  (setf collect-p t))
                 (t
                  (setf named-p (string-equal line (concatenate 'string "#+name: " name)))))))
    (onboarding-source--fail (format nil "Missing or unclosed text block ~S" name))))

(-> onboarding-pages (configuration) list)
(defun onboarding-pages (configuration)
  "Read the current lesson snapshot and interpolate CONFIGURATION's paths."
  (let ((variables (make-hash-table :test #'eq)))
    (setf (gethash ':config-root variables) (namestring (config :config-root configuration))
          (gethash ':state-root variables) (namestring (config :state-root configuration)))
    (onboarding-source-pages (onboarding-source--read) variables)))
