(in-package #:autolith)

;;;; -- Fullscreen Onboarding --

(define-condition onboarding-error (terminal-error)
  ()
  (:documentation "A fullscreen onboarding presentation could not complete."))


;;;; -- Page Validation --

(-> terminal-ui-onboarding-page-p (t) boolean)
(defun terminal-ui-onboarding-page-p (page)
  "Return true when PAGE has the supported onboarding page shape."
  (and (listp page)
       (evenp (length page))
       (non-empty-string-p (getf page :id))
       (typep (getf page :version) '(integer 1))
       (non-empty-string-p (getf page :title))
       (or (stringp (getf page :body))
           (terminal-styled-text-p (getf page :body)))
       (not (null (member (getf page :exercise)
                          '(nil :continue :help :async :settings :authentication) :test #'eq)))))

(-> terminal-ui-onboarding--validate-pages (list) null)
(defun terminal-ui-onboarding--validate-pages (pages)
  "Reject malformed PAGES before acquiring fullscreen input ownership."
  (unless (listp pages)
    (error 'onboarding-error
           :message "Onboarding pages must be a list."
           :operation ':onboard :cause nil))
  (let ((identifiers (make-hash-table :test #'equal)))
    (dolist (page pages)
      (unless (terminal-ui-onboarding-page-p page)
        (error 'onboarding-error
               :message "An onboarding page is malformed."
               :operation ':onboard :cause nil))
      (let ((identifier (getf page :id)))
        (when (gethash identifier identifiers)
          (error 'onboarding-error
                 :message (format nil "Onboarding page identifier ~S is repeated." identifier)
                 :operation ':onboard :cause nil))
        (setf (gethash identifier identifiers) t))))
  nil)


;;;; -- Rendered Page Layout --

(-> terminal-ui-onboarding--spans ((or string list)) list)
(defun terminal-ui-onboarding--spans (body)
  "Return BODY as trusted styled spans after page validation."
  (if (stringp body)
      (list (terminal-span ':plain body))
      body))


(-> terminal-ui-onboarding--link-action (list integer integer) t)
(defun terminal-ui-onboarding--link-action (regions column row)
  "Return the widget action at one-based terminal COLUMN and ROW."
  (loop for (region-row start end action) in regions
        when (and (= (1- row) region-row) (<= start (1- column)) (< (1- column) end))
          return action))

(-> terminal-ui-onboarding--wrap-spans (terminal list integer) list)
(defun terminal-ui-onboarding--wrap-spans (terminal spans width)
  "Wrap SPANS into styled physical rows no wider than WIDTH cells."
  (let* ((text (terminal--spans-text spans))
         (display (terminal--render-spans terminal spans)))
    (mapcar #'second (clinedi:wrap-styled-text text display (max 1 width)))))

(-> terminal-ui-onboarding--prompt-rows (terminal-ui integer) (values list integer integer))
(defun terminal-ui-onboarding--prompt-rows (ui width)
  "Return UI's existing editor rows and its cursor location for an exercise."
  (multiple-value-bind (rows cursor-row cursor-offset) (terminal-ui--composer-rows ui)
    (multiple-value-bind (text display cursor)
        (terminal-ui--rows-content (terminal-ui-terminal ui) rows
                                  :cursor-row cursor-row :cursor-offset cursor-offset)
      (multiple-value-bind (wrapped row column)
          (clinedi:wrap-styled-editor-rows text display :cursor cursor :columns (max 1 width))
        (values (mapcar #'second wrapped) row column)))))

(-> terminal-ui-onboarding--footer
    (integer integer &key (:width integer) (:input-p boolean) (:scrollable-p boolean))
    list)
(defun terminal-ui-onboarding--footer (index count &key (width 76) input-p scrollable-p)
  "Choose a fitting legend, prioritizing scrolling help when space is limited."
  (let ((variables (make-hash-table :test #'eq))
        (names (if scrollable-p
                   '("footer-wide" "footer-scroll-compact" "footer-scroll-small"
                     "footer-small" "footer-minimal")
                   '("footer-wide" "footer-medium" "footer-compact"
                     "footer-small" "footer-minimal"))))
    (setf (gethash ':current variables) (1+ index)
          (gethash ':count variables) count
          (gethash ':action variables) (onboarding-source-text (if input-p "action-check" "action-next"))
          (gethash ':scroll variables) (if scrollable-p (onboarding-source-text "footer-scroll") ""))
    (loop for name in names
          for spans = (mapcar (lambda (fragment)
                                (if (stringp fragment)
                                    (terminal-span ':dim fragment)
                                    (terminal-span (first fragment) (second fragment))))
                              (onboarding-source--inline
                               (onboarding-source-text name variables)))
          when (or (<= (text-cell-width (terminal--spans-text spans)) width)
                   (string= name "footer-minimal"))
            return spans)))

(-> terminal-ui-onboarding--exercise-hint ((option keyword) keyword) string)
(defun terminal-ui-onboarding--exercise-hint (exercise stage)
  "Return one authored instruction for EXERCISE at STAGE."
  (let ((name (case exercise
                (:continue "hint-continue")
                (:help (if (eq stage ':help-output) "hint-help-output" "hint-help-input"))
                (:async (if (eq stage ':result) "hint-async-result" "hint-async-input"))
                (:settings (if (eq stage ':result) "hint-settings-result" "hint-settings-input"))
                (:authentication
                 (case stage
                   (:result "hint-auth-result")
                   (:failed "hint-auth-failed")
                   (:cancelled "hint-auth-cancelled")
                   (otherwise "hint-auth-input"))))))
    (if name
        (onboarding-source-text name)
        "")))

(-> terminal-ui-onboarding--exercise-target ((option keyword) keyword) (option string))
(defun terminal-ui-onboarding--exercise-target (exercise stage)
  "Return the submission target for EXERCISE at STAGE."
  (case exercise
    (:continue "continue")
    (:help (if (eq stage ':help-output) "continue" "(help)"))
    (:async (unless (eq stage ':result) "? (+ 1 2)"))
    (:settings (unless (eq stage ':result) "(settings)"))
    (:authentication (unless (eq stage ':result) "(auth)"))
    (otherwise nil)))

(-> terminal-ui-onboarding--help-spans (function &key (:width integer)) list)
(defun terminal-ui-onboarding--help-spans (help-function &key (width 76))
  "Render HELP-FUNCTION's Markdown output or accept its existing styled spans."
  (let ((output (funcall help-function)))
    (cond
      ((stringp output)
       (let ((renderer (termdown:markdown-renderer-create :width width)))
         (with-input-from-string (stream output)
           (loop for line = (read-line stream nil nil)
                 while line
                 append (loop for row in (terminal-markdown-render-line renderer line)
                              append (append row (list (terminal-span ':plain (string #\Newline)))))))))
      ((terminal-styled-text-p output)
       output)
      (t
       (error 'onboarding-error
              :message "The onboarding help function must return text or terminal spans."
              :operation ':onboard :cause nil)))))

(-> terminal-ui-onboarding--center-logo (list integer) list)
(defun terminal-ui-onboarding--center-logo (spans width)
  "Center all Lambda Symbolics logo rows as one block within WIDTH cells."
  (let* ((logo-spans (remove-if-not
                      (lambda (span)
                        (and (terminal-span-p span)
                             (member (terminal-span-style span) *terminal-lambda-logo-names*)))
                      spans))
         (logo-width (loop for span in logo-spans
                           maximize (loop for line in (uiop:split-string (terminal-span-text span)
                                                                        :separator '(#\Newline))
                                          maximize (text-cell-width line)) into maximum
                           finally (return (or maximum 0))))
         (padding (make-string (max 0 (floor (- width logo-width) 2)) :initial-element #\Space)))
    (loop for span in spans
          collect (if (member span logo-spans :test #'eq)
                      (terminal-span
                       (terminal-span-style span)
                       (format nil "~{~A~^~%~}"
                               (mapcar (lambda (line) (concatenate 'string padding line))
                                       (uiop:split-string (terminal-span-text span)
                                                          :separator '(#\Newline)))))
                      span))))

(-> terminal-ui-onboarding--body-spans (list keyword function &key (:width integer)) list)
(defun terminal-ui-onboarding--body-spans (page stage help-function &key (width 76))
  "Return the current lesson or the result of its exercise."
  (if (and (eq (getf page :exercise) ':help) (member stage '(:help-output :help-error)))
      (append (list (terminal-span ':strong (format nil "~A~2%" (onboarding-source-text "help-title"))))
              (terminal-ui-onboarding--help-spans help-function :width width))
      (append
       (terminal-ui-onboarding--center-logo
        (terminal-ui-onboarding--spans (getf page :body)) width)
       (when (eq (getf page :exercise) ':async)
         (case stage
           (:async-running
            (list (terminal-span ':notice (format nil "~2%~A" (onboarding-source-text "async-running")))))
           (:result
            (list (terminal-span ':success (format nil "~2%~A" (onboarding-source-text "async-result"))))))))))

(-> terminal-ui-onboarding--async-demo (function) integer)
(defun terminal-ui-onboarding--async-demo (pending-function)
  "Compute the fixed (+ 1 2) lesson in a joined thread, with one pending repaint."
  (let ((thread (bordeaux-threads:make-thread (lambda () (+ 1 2))
                                             :name "Autolith onboarding Lisp demo"))
        (joined-p nil))
    (unwind-protect
         (progn
           (funcall pending-function)
           (prog1 (bordeaux-threads:join-thread thread)
             (setf joined-p t)))
      (unless joined-p
        (bordeaux-threads:join-thread thread)))))

(-> terminal-ui-onboarding--frame
    (fullscreen-terminal-ui list
     &key (:index integer) (:count integer) (:stage keyword)
          (:help-function function) (:introduction-title string) (:scroll-offset integer))
    (values list integer integer integer list))
(defun terminal-ui-onboarding--frame
    (ui page &key (index 0) (count 1) (stage ':input) (help-function (constantly ""))
                  (introduction-title "") (scroll-offset 0))
  "Compose a centered reading card, pinned real editor, and widget hit regions.

Body scrolling never hides the exercise prompt. Recompute geometry each paint."
  (let* ((terminal (terminal-ui-terminal ui))
         (width (max 1 (terminal-columns terminal)))
         (height (max 1 (terminal-rows terminal)))
         (card-width (min 76 (max 1 (- width (if (> width 10) 4 0)))))
         (left (floor (- width card-width) 2))
         (exercise (getf page :exercise))
         (spans (terminal-ui-onboarding--body-spans page stage help-function :width card-width))
         (text (terminal--spans-text spans))
         (wrapped (clinedi:wrap-styled-text text (terminal--render-spans terminal spans) card-width))
         (title (getf page :title))
         (title-rows (terminal-ui-onboarding--wrap-spans
                      terminal (list (terminal-span ':strong title)) card-width))
         (header (when (>= height 8)
                   (append
                    (list (terminal--render-spans
                           terminal
                           (list (terminal-span ':brand
                                                (format nil "AUTOLITH  ~A~A"
                                                        (make-string (1+ index) :initial-element #\●)
                                                        (make-string (- count index 1) :initial-element #\○)))))
                          (if (and (zerop index) (non-empty-string-p introduction-title))
                              (terminal--render-spans
                               terminal (list (terminal-span ':dim introduction-title)))
                              ""))
                    title-rows
                    (list (terminal--render-spans
                           terminal (list (terminal-span ':hint
                                                        (make-string card-width :initial-element #\─))))))))
         (hint (terminal-ui-onboarding--exercise-hint exercise stage))
         (regions nil))
    (multiple-value-bind (prompt-rows prompt-row prompt-column)
        (if exercise (terminal-ui-onboarding--prompt-rows ui card-width) (values nil 0 0))
      (let* ((hint-rows (when exercise
                          (terminal-ui-onboarding--wrap-spans
                           terminal (list (terminal-span ':strong hint)) card-width)))
             (footer (first (terminal-ui-onboarding--wrap-spans
                             terminal
                             (terminal-ui-onboarding--footer
                              index count :width card-width
                              :input-p (not (null (terminal-ui-onboarding--exercise-target exercise stage)))
                              :scrollable-p t)
                             card-width)))
             (prompt-height (max 1 (- height (length header)
                                     (if (and exercise (>= height 6)) (length hint-rows) 0)
                                     (if (> height 1) 1 0))))
             (prompt-top (min (max 0 (- prompt-row (1- prompt-height)))
                              (max 0 (- (length prompt-rows) prompt-height))))
             (visible-prompt (subseq prompt-rows prompt-top
                                     (min (length prompt-rows) (+ prompt-top prompt-height))))
             (bottom (append (when (and exercise (>= height 6)) hint-rows)
                             visible-prompt (when (> height 1) (list footer))))
             (body-height (max 0 (- height (length header) (length bottom))))
             (maximum-offset (max 0 (- (length wrapped) body-height)))
             (offset (min (max 0 scroll-offset) maximum-offset))
             (shown (subseq wrapped offset (min (length wrapped) (+ offset body-height))))
             (padding (floor (max 0 (- body-height (length shown))) 2))
             (source-offset 0)
             (widget-regions (termdown:widget-regions spans))
             (body-row (+ (length header) padding)))
        ;; Source offsets survive omitted boundary whitespace during word wrap.
        (loop for (plain display) in wrapped
              for row from 0
              for start = (or (search plain text :start2 source-offset) source-offset)
              for end = (+ start (length plain))
              do (setf source-offset end)
                 (when (and (<= offset row) (< row (+ offset body-height)))
                   (loop for (region-start region-end action) in widget-regions
                         for a = (max start region-start)
                         for b = (min end region-end)
                         when (< a b)
                           do (push (list (+ body-row (- row offset))
                                          (+ left (text-cell-width text :start start :end a))
                                          (+ left (text-cell-width text :start start :end b)) action)
                                    regions))))
        (let* ((content (append header
                                (make-list padding :initial-element "")
                                (mapcar #'second shown)
                                (make-list (max 0 (- body-height padding (length shown))) :initial-element "")
                                bottom))
               (overflow (max 0 (- (length content) height)))
               (rows (subseq content overflow))
               (cursor-row (- (+ (- (length content) (length bottom))
                                 (if (and exercise (>= height 6)) (length hint-rows) 0)
                                 (- prompt-row prompt-top)) overflow)))
          (values
           (loop for row in rows
                 for row-index from 0
                 for display = (second (first (clinedi:wrap-styled-text
                                               (clinedi:ansi-strip row) row card-width)))
                 for margin = (if (and (> height 1) (= row-index (1- (length rows))))
                                  (floor (- width (text-cell-width (clinedi:ansi-strip display))) 2)
                                  left)
                 collect (concatenate 'string (make-string (max 0 margin) :initial-element #\Space)
                                      display))
           (min (1- height) (max 0 cursor-row))
           (min (1- width) (+ left prompt-column))
           offset
           (loop for (row start end action) in regions
                 when (<= overflow row)
                   collect (list (- row overflow) start end action))))))))

(-> terminal-ui-onboarding--paint
    (fullscreen-terminal-ui list
     &key (:index integer) (:stage keyword) (:help-function function)
          (:introduction-title string) (:scroll-offset integer))
    (values integer list))
(defun terminal-ui-onboarding--paint
    (ui pages &key (index 0) (stage ':input) (help-function (constantly ""))
                   (introduction-title "") (scroll-offset 0))
  "Paint the card and return bounded scroll offset and widget regions."
  (multiple-value-bind (rows cursor-row cursor-column offset regions)
      (terminal-ui-onboarding--frame
       ui (nth index pages) :index index :count (length pages) :stage stage
       :help-function help-function :introduction-title introduction-title
       :scroll-offset scroll-offset)
    (setf (fullscreen-terminal-ui-cursor-visible-p ui)
          (not (null (getf (nth index pages) :exercise))))
    (terminal-ui-fullscreen-paint ui :rows rows :cursor-row cursor-row :cursor-column cursor-column)
    (values offset regions)))


;;;; -- Exercise Input --

(-> terminal-ui-onboarding--copy-attachments (list) list)
(defun terminal-ui-onboarding--copy-attachments (attachments)
  "Return a detached copy of editor ATTACHMENTS for restoration after onboarding."
  (loop for attachment in attachments
        collect (cons (copy-seq (first attachment)) (rest attachment))))

(-> terminal-ui-onboarding--reset-exercise (terminal-ui) null)
(defun terminal-ui-onboarding--reset-exercise (ui)
  "Clear only temporary demo input before entering another onboarding page."
  (line-editor-clear (terminal-ui-editor ui))
  (setf (terminal-ui-image-attachments ui) nil
        (terminal-ui-lisp-input-p ui) nil
        (terminal-ui-prompt-render-cache ui) nil)
  nil)

(-> terminal-ui-onboarding--apply-exercise-event (terminal-ui t) boolean)
(defun terminal-ui-onboarding--apply-exercise-event (ui event)
  "Edit freely through the real editor without submitting application work."
  (let* ((editor (terminal-ui-editor ui))
         (safe-event (terminal-ui--safe-editor-event event))
         (command (clinedi:line-editor-command-for-event editor safe-event)))
    (when (member command '(:insert :paste :insert-newline :left :right :home :end
                           :backspace :delete :kill-line :kill-to-end :kill-word
                           :kill-word-right :word-left :word-right
                           :history-previous :history-next :toggle-word-delimiter-mode
                           :up :down) :test #'eq)
      (if (member command '(:up :down) :test #'eq)
          (unless (line-editor-move-vertical
                   editor (if (eq command ':up) -1 1)
                   :columns (let ((width (terminal-columns (terminal-ui-terminal ui))))
                              (min 76 (max 1 (- width (if (> width 10) 4 0)))))
                   :prompt-width (text-cell-width (terminal-ui-prompt ui)))
            (line-editor-handle-event editor (if (eq command ':up) ':history-previous ':history-next)))
          (line-editor-handle-event editor safe-event))
      (setf (terminal-ui-prompt-render-cache ui) nil)
      t)))

(-> terminal-ui-onboarding--exercise-submitted-p (terminal-ui string) boolean)
(defun terminal-ui-onboarding--exercise-submitted-p (ui target)
  "Return true only when UI's demo editor equals TARGET on Enter."
  (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (line-editor-text (terminal-ui-editor ui)))
           target))

(-> terminal-ui-onboarding--help-request (string) (values t boolean))
(defun terminal-ui-onboarding--help-request (input)
  "Decode only help requests, returning a native topic and a valid-request flag.

Accept the help command's literal Lisp arguments and its slash spelling. Disable
reader dispatch and never evaluate input, including quoted topic arguments."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
    (when (and (>= (length text) 5)
               (string-equal "/help" text :end2 5)
               (or (= (length text) 5) (find (char text 5) '(#\Space #\Tab))))
      (let ((topic (string-trim '(#\Space #\Tab) (subseq text 5))))
        (return-from terminal-ui-onboarding--help-request
          (values (unless (zerop (length topic)) topic) t))))
    (handler-case
        (let ((*read-eval* nil)
              (*readtable* (copy-readtable nil))
              (*package* (find-package '#:autolith))
              (end (gensym "END")))
          (set-macro-character #\#
                               (lambda (stream character)
                                 (declare (ignore character))
                                 (error 'reader-error :stream stream)))
          (with-input-from-string (stream text)
            (let ((form (read stream nil end)))
              (when (and (listp form) (<= 1 (length form) 2)
                         (symbolp (first form))
                         (string-equal (symbol-name (first form)) "HELP")
                         (eq (read stream nil end) end))
                (let* ((argument (second form))
                       (quoted-p (and (consp argument) (eq (first argument) 'quote)
                                      (= (length argument) 2)))
                       (topic (if quoted-p (second argument) argument)))
                  (when (or (null topic) (stringp topic) (keywordp topic)
                            (and quoted-p (symbolp topic)))
                    (return-from terminal-ui-onboarding--help-request
                      (values topic t))))))))
      (error () nil)))
  (values nil nil))


;;;; -- Modal Session --

(-> terminal-ui-onboard
    (terminal-ui list &key (:help-function function) (:settings-function (option function))
                         (:authentication-function (option function))
                         (:page-completed-function function) (:introduction-title string)
                         (:resize-callback (option function)) (:wait-function function))
    (values keyword list))
(defun terminal-ui-onboard
    (ui pages &key (help-function (constantly "")) settings-function authentication-function
                  (page-completed-function (constantly nil))
                  (introduction-title "") resize-callback (wait-function #'sleep))
  "Present fullscreen PAGES with safe exercises through the real prompt editor.

Return outcome :COMPLETED, :REFUSED (Escape), or :INTERRUPTED (Ctrl-C/EOF),
and completed page IDs. Call PAGE-COMPLETED-FUNCTION once per page, with its
plist. Hold exclusive input through snapshot, presentation and restoration;
defer concurrent live output without dropping it. Only check exercises on Enter.
Call HELP-FUNCTION once per accepted help request, with its optional native topic;
cache its output across paints. Call SETTINGS-FUNCTION for the settings lesson.
Call AUTHENTICATION-FUNCTION for provider selection and login; its outcome is
:AUTHENTICATED, :SKIPPED, :CANCELLED, or :FAILED. Only the first two finish the
exercise; cancellation and failure allow retry without acknowledging the page.
Poll RESIZE-CALLBACK while idle; it returns pending dimensions as (ROWS . COLUMNS).
Never evaluate demo input."
  (terminal-ui-onboarding--validate-pages pages)
  (unless (and (typep ui 'fullscreen-terminal-ui)
               (functionp help-function)
               (or (null settings-function) (functionp settings-function))
               (or (null authentication-function) (functionp authentication-function))
               (functionp wait-function)
               (or (null resize-callback) (functionp resize-callback))
               (or settings-function
                   (notany (lambda (page) (eq (getf page :exercise) ':settings)) pages))
               (or authentication-function
                   (notany (lambda (page) (eq (getf page :exercise) ':authentication)) pages))
               (functionp page-completed-function)
               (stringp introduction-title))
    (error 'onboarding-error
           :message "Fullscreen onboarding needs a fullscreen UI, functions, and a title string."
           :operation ':onboard :cause nil))
  (when (null pages)
    (return-from terminal-ui-onboard (values ':completed nil)))
  (terminal-ui-call-with-exclusive-input
   ui
   (lambda ()
     (let* ((*onboarding-source-snapshot* (onboarding-source--read))
            (terminal (terminal-ui-terminal ui))
            (saved-editor (clinedi:line-editor-snapshot (terminal-ui-editor ui)))
            (saved-attachments (terminal-ui-onboarding--copy-attachments
                                (terminal-ui-image-attachments ui)))
            (saved-lisp-input-p (terminal-ui-lisp-input-p ui))
            (saved-selector (terminal-ui-selector ui))
            (saved-completion-dismissed-p (terminal-ui-completion-dismissed-p ui))
            (saved-output-suspended-p (terminal-ui-live-output-suspended-p ui))
            (saved-cursor-visible-p (fullscreen-terminal-ui-cursor-visible-p ui))
            (owned-fullscreen-p (not (fullscreen-terminal-ui-active-p ui)))
            (index 0)
            (scroll-offset 0)
            (link-regions nil)
            (stage ':input)
             (help-output "")
             (help-seen-p nil)
            (seen-identifiers nil)
            (outcome ':running))
       (labels ((page ()
                  (nth index pages))

                (exercise ()
                  (getf (page) :exercise))

                (paint ()
                  (with-terminal-ui-locked (ui)
                    (multiple-value-setq (scroll-offset link-regions)
                      (terminal-ui-onboarding--paint
                       ui pages :index index :stage stage :help-function (lambda () help-output)
                       :introduction-title introduction-title :scroll-offset scroll-offset))))

                (reset-page ()
                  (setf scroll-offset 0 stage ':input help-output "" help-seen-p nil)
                  (when (exercise)
                    (terminal-ui-onboarding--reset-exercise ui)))

                (advance ()
                  (let ((identifier (getf (page) :id)))
                    (unless (member identifier seen-identifiers :test #'equal)
                      (funcall page-completed-function (page))
                      (push identifier seen-identifiers)))
                  (if (= index (1- (length pages)))
                      (setf outcome ':completed)
                      (progn (incf index) (reset-page))))

                (back ()
                  (when (plusp index)
                    (decf index)
                    (reset-page)))

                (submit-help ()
                  (cond
                    ((and help-seen-p
                          (terminal-ui-onboarding--exercise-submitted-p ui "continue"))
                     (advance))
                    (t
                     (multiple-value-bind (topic valid-p)
                         (terminal-ui-onboarding--help-request
                          (line-editor-text (terminal-ui-editor ui)))
                       (setf scroll-offset 0)
                       (if valid-p
                           (progn
                             (setf help-output (if topic
                                                   (funcall help-function topic)
                                                   (funcall help-function))
                                   help-seen-p t
                                   stage ':help-output)
                             (terminal-ui-onboarding--reset-exercise ui))
                           (setf help-output (onboarding-source-text "help-input-error")
                                 stage (if help-seen-p ':help-output ':help-error)))))))

                (submit-exercise (target)
                  (when (terminal-ui-onboarding--exercise-submitted-p ui target)
                    (case (exercise)
                      (:authentication
                       (terminal-ui-onboarding--reset-exercise ui)
                       (setf stage
                             (case (funcall authentication-function)
                               ((:authenticated :skipped) ':result)
                               (:cancelled ':cancelled)
                               (otherwise ':failed))
                             scroll-offset 0)
                       (terminal-ui-onboarding--reset-exercise ui))
                      (:settings
                       (terminal-ui-onboarding--reset-exercise ui)
                       (funcall settings-function)
                       (setf stage ':result scroll-offset 0)
                       (terminal-ui-onboarding--reset-exercise ui))
                      (:async
                       (let ((result
                               (terminal-ui-onboarding--async-demo
                                (lambda ()
                                  (setf stage ':async-running)
                                  (paint)))))
                         (unless (eql result 3)
                           (error 'onboarding-error
                                  :message "The fixed Lisp demo returned an unexpected result."
                                  :operation ':onboard :cause nil)))
                       (setf stage ':result scroll-offset 0)
                       (terminal-ui-onboarding--reset-exercise ui))
                      (otherwise
                       (advance)))))

                (handle-event (event)
                  (let ((target (terminal-ui-onboarding--exercise-target (exercise) stage)))
                    (cond
                      ((member event '(:interrupt :end-of-input :stream-end) :test #'eq)
                       (setf outcome ':interrupted))
                      ((eq event ':escape)
                       (setf outcome ':refused))
                      ((member event '(:back :complete-previous) :test #'eq)
                       (back))
                      ((and (eq event ':left) (not target))
                       (back))
                      ((or (eq event ':page-up) (and (eq event ':up) (not target)))
                       (setf scroll-offset (max 0 (- scroll-offset (if (eq event ':page-up) 5 1)))))
                      ((or (eq event ':page-down) (and (eq event ':down) (not target)))
                       (incf scroll-offset (if (eq event ':page-down) 5 1)))
                      ((and (consp event) (eq (first event) ':scroll))
                       (incf scroll-offset (* 3 (second event))))
                      ((and (consp event) (eq (first event) ':resize))
                       (terminal-ui-resize ui (second event) :rows (third event)))
                      ((and (consp event) (eq (first event) ':click))
                       (let ((action (terminal-ui-onboarding--link-action
                                      link-regions (second event) (third event))))
                         (when action (terminal-ui-activate ui action))))
                      ((and (eq (exercise) ':help) (eq event ':submit))
                       (submit-help))
                      ((and (eq (exercise) ':authentication) (eq event ':submit)
                            (terminal-ui-onboarding--exercise-submitted-p ui "continue"))
                       (advance))
                      ((and target (eq event ':submit))
                       (submit-exercise target))
                      (target
                       (terminal-ui-onboarding--apply-exercise-event ui event))
                      ((or (member event '(:submit :right) :test #'eq)
                           (equal event '(:insert " ")))
                       (advance))))))
         (unwind-protect
              (progn
                (with-terminal-ui-locked (ui)
                  (setf (terminal-ui-live-output-suspended-p ui) t
                        (terminal-ui-selector ui) nil
                        (terminal-ui-completion-dismissed-p ui) t)
                  (when owned-fullscreen-p
                    (terminal-ui-fullscreen-enter ui))
                  (unless (fullscreen-terminal-ui-active-p ui)
                    (error 'onboarding-error
                           :message "Fullscreen onboarding requires an interactive terminal."
                           :operation ':onboard :cause nil))
                  (reset-page))
                (paint)
                (loop while (eq outcome ':running)
                      do (when (terminal-ui-refresh-size ui resize-callback)
                           (paint))
                         (if (terminal-input-ready-p terminal)
                             (progn
                               (handle-event (terminal-read-event terminal))
                               (when (eq outcome ':running)
                                 (paint)))
                             (funcall wait-function *terminal-ui-picker-poll-interval*))))
           (with-terminal-ui-locked (ui)
             (clinedi:line-editor-restore (terminal-ui-editor ui) saved-editor)
             (setf (terminal-ui-image-attachments ui) saved-attachments
                   (terminal-ui-lisp-input-p ui) saved-lisp-input-p
                   (terminal-ui-selector ui) saved-selector
                   (terminal-ui-completion-dismissed-p ui) saved-completion-dismissed-p
                   (terminal-ui-live-output-suspended-p ui) saved-output-suspended-p
                   (terminal-ui-prompt-render-cache ui) nil
                   (fullscreen-terminal-ui-cursor-visible-p ui) saved-cursor-visible-p)
             (when owned-fullscreen-p
               (terminal-ui-fullscreen-leave ui))
             (when (and (terminal-ui-started-p ui)
                        (fullscreen-terminal-ui-active-p ui)
                        (not saved-output-suspended-p))
               (terminal-ui--paint-live ui))))
         (values outcome (nreverse seen-identifiers)))))))
