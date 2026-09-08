(in-package #:autolith)

;;;; -- UI Construction --

(-> terminal-ui--maximum-live-rows (terminal) (integer 1))
(defun terminal-ui--maximum-live-rows (terminal)
  "Return the viewport row budget reserved for TERMINAL's unfinished content."
  (max 1 (1- (terminal-rows terminal))))

(defparameter *terminal-ui-stale-status-seconds* 30
  "The idle duration after which live activity is labelled as stale.")

(defparameter *terminal-ui-status-spinner-frames-per-second* 4
  "The number of live status animation frames painted each second.")

(defparameter *terminal-ui-context-track-cells* 12
  "The preferred number of context-capacity cells beside the compaction marker.")

(defparameter *terminal-ui-compaction-track-cells* 32
  "The maximum width of the indeterminate compaction track.")

(defparameter *terminal-ui-compaction-head-cells* 5
  "The maximum width of the travelling compaction head.")

(defparameter *terminal-ui-picker-search-character-limit* 256
  "The maximum number of characters retained in a modal picker search.")

(defparameter *terminal-ui-picker-poll-interval* 0.02
  "The default seconds between modal picker terminal-readiness checks.")

(defparameter *terminal-ui-pending-preview-limit* 3
  "The maximum pending inputs previewed for each delivery class.")

(defparameter *terminal-ui-agent-visible-limit* 8
  "The maximum queued and running child agents shown above the modeline.")

(defparameter *terminal-ui-command-visible-limit* 8
  "The maximum queued and running primary commands shown above the modeline.")

(defparameter *terminal-ui-command-pending-completion-limit* 64
  "The maximum completed primary commands retained until their first paint.")

(defparameter *terminal-ui-agent-compact-trace-limit* 3
  "The child tool milestones retained on one compact activity row.")

(defparameter *terminal-ui-agent-expanded-trace-limit* 6
  "The child tool milestones shown on one blocking activity trace row.")

(defparameter *terminal-ui-agent-spinner-frames*
  #("⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷")
  "The shared running-child spinner cycle.")

(-> terminal-ui--monotonic-seconds () real)
(defun terminal-ui--monotonic-seconds ()
  "Return monotonic process time in seconds for live activity accounting."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(-> terminal-ui--raw-spans-text (list) string)
(defun terminal-ui--raw-spans-text (spans)
  "Return the unsanitized text represented by SPANS."
  (with-output-to-string (stream)
    (dolist (span spans)
      (write-string (terminal-span-text span) stream))))


(-> terminal-completion-p (t) boolean)
(defun terminal-completion-p (value)
  "Return true when VALUE describes one interactive completion entry."
  (let ((description (and (listp value) (getf value :description)))
        (description-spans (and (listp value)
                                (getf value :description-spans))))
    (and (listp value)
         (non-empty-string-p (getf value :name))
         (typep (getf value :argument) '(option string))
         (typep (getf value :value) '(option string))
         (stringp description)
         (or (null description-spans)
             (and (terminal-styled-text-p description-spans)
                  (string= description
                           (terminal-ui--raw-spans-text
                            description-spans)))))))

(-> terminal-agent-activity-p (t) boolean)
(defun terminal-agent-activity-p (value)
  "Return true when VALUE is one queued or running child presentation."
  (handler-case
      (let ((recent-tools (and (listp value)
                               (getf value :recent-tools))))
        (not
         (null
          (and (listp value)
               (evenp (length value))
               (non-empty-string-p (getf value :id))
               (typep (getf value :index) '(integer 1))
               (non-empty-string-p (getf value :agent))
               (member (getf value :state) '(:queued :running) :test #'eq)
               (typep (getf value :current-tool) '(option string))
               (typep (getf value :current-tool-duration-ms)
                      '(option (integer 0)))
               (listp recent-tools)
               (<= (length recent-tools)
                   *task-progress-recent-tool-limit*)
               (every #'stringp recent-tools)
               (typep (getf value :request-count) '(integer 0))
               (typep (getf value :duration-ms) '(option (integer 0)))
               (stringp (getf value :assignment))
               (typep (getf value :detached) 'boolean)))))
    (error ()
      nil)))

(-> terminal-command-activity-p (t) boolean)
(defun terminal-command-activity-p (value)
  "Return true when VALUE is one queued or running primary command presentation."
  (handler-case
      (not
       (null
        (and (listp value)
             (evenp (length value))
             (eq (getf value :type) ':tool)
             (non-empty-string-p (getf value :id))
             (typep (getf value :index) '(integer 1))
             (non-empty-string-p (getf value :tool))
             (non-empty-string-p (getf value :description))
             (member (getf value :state) '(:queued :running) :test #'eq)
             (typep (getf value :duration-ms) '(option (integer 0)))
             (typep (getf value :detached) 'boolean))))
    (error ()
      nil)))

(-> terminal-ui-create
    (&key (:terminal terminal) (:editor (option line-editor)) (:prompt string)
          (:placeholder string) (:completions list)
          (:completion-function (option function))
          (:clock-function function))
    terminal-ui)
(defun terminal-ui-create
    (&key terminal editor (prompt "> ") (placeholder "") completions
          completion-function
          (clock-function #'terminal-ui--monotonic-seconds))
  "Create a scrollback-preserving UI for TERMINAL."
  (unless (typep terminal 'terminal)
    (error 'terminal-error
           :message "TERMINAL-UI-CREATE requires a terminal instance."
           :operation ':create-ui
           :cause nil))
  (unless (every #'terminal-completion-p completions)
    (error 'terminal-error
           :message "Every completion entry needs a name and a description."
           :operation ':create-ui
           :cause nil))
  (unless (typep completion-function '(option function))
    (error 'terminal-error
           :message "The completion provider must be a function or NIL."
           :operation ':create-ui
           :cause nil))
  (let ((live-region
          (make-live-region
           :columns (terminal-columns terminal)
           :maximum-rows (terminal-ui--maximum-live-rows terminal)
           :write-function (lambda (text)
                             (terminal--write terminal text))
           :flush-function (lambda ()
                             (terminal-flush terminal)))))
    (make-instance 'terminal-ui
                   :terminal terminal
                   :editor (or editor
                               (line-editor-create
                                :history-limit *terminal-history-limit*))
                   :live-region live-region
                   :clock-function clock-function
                   :prompt prompt
                   :placeholder placeholder
                   :completions completions
                   :completion-function completion-function
                   :completion-selector
                   (make-selector
                    :visible-count *terminal-ui-visible-completions*
                    :arrangement ':vertical))))


(defmacro with-terminal-ui-locked ((ui) &body body)
  "Run BODY while holding UI's recursive presentation lock."
  (let ((locked-ui (gensym "UI")))
    `(let ((,locked-ui ,ui))
       (with-recursive-lock-held ((terminal-ui-lock ,locked-ui))
         ,@body))))


(-> terminal-ui-load-history (terminal-ui list) terminal-ui)
(defun terminal-ui-load-history (ui entries)
  "Replace UI's editable history while preserving its current draft and cursor."
  (with-terminal-ui-locked (ui)
    (let* ((editor (terminal-ui-editor ui))
           (navigating-p
             (not
              (null (slot-value editor 'clinedi::history-index))))
           (text
             (copy-seq
              (if navigating-p
                  (slot-value editor 'clinedi::history-stash)
                  (line-editor-text editor))))
           (cursor
             (if navigating-p
                 (slot-value editor 'clinedi::history-stash-cursor)
                 (line-editor-cursor editor)))
           (history-editor
             (line-editor-create
              :history (mapcar #'sanitize-text entries)
              :history-limit (line-editor-history-limit editor))))
      (line-editor-set-text editor text :cursor cursor)
      (setf (slot-value editor 'clinedi::history)
            (slot-value history-editor 'clinedi::history))))
  ui)

(-> terminal-ui--call-with-lock-if-available
    (terminal-ui function)
    (values t boolean))
(defun terminal-ui--call-with-lock-if-available (ui function)
  "Call FUNCTION under UI's lock when it is immediately available.

The second value reports whether FUNCTION ran. This nonblocking path keeps
emergency terminal input responsive while another thread owns presentation."
  (let ((lock (terminal-ui-lock ui)))
    (sb-sys:without-interrupts
      (if (sb-thread:grab-mutex lock :waitp nil)
          (unwind-protect
               (sb-sys:with-interrupts
                 (values (funcall function) t))
            (sb-thread:release-mutex lock))
        (values nil nil)))))


;;;; -- Terminal Presentation --

(-> terminal-ui--image-label (integer) string)
(defun terminal-ui--image-label (number)
  "Return the visible composer label for image NUMBER."
  (format nil "[Image #~D]" number))

(-> terminal-ui--copy-image-attachments (list) list)
(defun terminal-ui--copy-image-attachments (attachments)
  "Return a detached copy of draft ATTACHMENTS."
  (loop for attachment in attachments
        collect (cons (copy-seq (first attachment))
                      (rest attachment))))

(-> terminal-ui--replace-image-labels (string list) string)
(defun terminal-ui--replace-image-labels (text mapping)
  "Replace image labels in TEXT according to simultaneous MAPPING."
  (with-output-to-string (stream)
    (loop with position = 0
          while (< position (length text))
          for replacement =
            (find-if
             (lambda (entry)
               (let ((label (first entry)))
                 (and (<= (+ position (length label)) (length text))
                      (string= label text
                               :start2 position
                               :end2 (+ position (length label))))))
             mapping)
          do (if replacement
                 (progn
                   (write-string (rest replacement) stream)
                   (incf position (length (first replacement))))
                 (progn
                   (write-char (char text position) stream)
                   (incf position))))))

(-> terminal-ui--submission-input (terminal-ui string)
    (or string user-message-input))
(defun terminal-ui--submission-input (ui text)
  "Return TEXT with only its surviving, consecutively labelled attachments."
  (let* ((attachments
           (remove-if-not
            (lambda (attachment)
              (search (first attachment) text))
            (terminal-ui-image-attachments ui)))
         (mapping
           (loop for attachment in attachments
                 for number from 1
                 collect (cons (first attachment)
                               (terminal-ui--image-label number))))
         (normalized-text (terminal-ui--replace-image-labels text mapping)))
    (if attachments
        (user-message-input-create
         :text normalized-text
         :image-pathnames (mapcar #'rest attachments))
        normalized-text)))

(-> terminal-ui--remember-image-submission (terminal-ui string list) null)
(defun terminal-ui--remember-image-submission (ui text attachments)
  "Remember submitted TEXT and ATTACHMENTS for Clinedi history recall."
  (when attachments
    (push (list :text (copy-seq text)
                :attachments
                (terminal-ui--copy-image-attachments attachments))
          (terminal-ui-image-history ui))
    (when (> (length (terminal-ui-image-history ui))
             *terminal-history-limit*)
      (setf (terminal-ui-image-history ui)
            (subseq (terminal-ui-image-history ui)
                    0 *terminal-history-limit*))))
  nil)

(-> terminal-ui--restore-history-images (terminal-ui) null)
(defun terminal-ui--restore-history-images (ui)
  "Restore or prune image attachments for UI's current editor text."
  (let ((text (line-editor-text (terminal-ui-editor ui))))
    (if (terminal-ui-image-attachments ui)
        (setf (terminal-ui-image-attachments ui)
              (remove-if-not
               (lambda (attachment)
                 (search (first attachment) text))
               (terminal-ui-image-attachments ui)))
        (let ((record
                (find text
                      (terminal-ui-image-history ui)
                      :key (lambda (entry) (getf entry :text))
                      :test #'string=)))
          (when record
            (setf (terminal-ui-image-attachments ui)
                  (terminal-ui--copy-image-attachments
                   (getf record :attachments)))))))
  nil)

(-> terminal-ui--attach-pasted-image (terminal-ui string) boolean)
(defun terminal-ui--attach-pasted-image (ui pasted-text)
  "Attach PASTED-TEXT when it names a supported local image."
  (let ((pathname (image-input-recognize-pasted-path pasted-text)))
    (if pathname
        (let ((label
                (terminal-ui--image-label
                 (1+ (length (terminal-ui-image-attachments ui))))))
          (line-editor-handle-event
           (terminal-ui-editor ui)
           (list ':insert label))
          (setf (terminal-ui-image-attachments ui)
                (nconc (terminal-ui-image-attachments ui)
                       (list (cons label pathname))))
          t)
        nil)))

(-> terminal-ui--set-draft-input
    (terminal-ui (or string user-message-input))
    null)
(defun terminal-ui--set-draft-input (ui input)
  "Replace UI's editor and attachment state with INPUT."
  (setf (terminal-ui-image-attachments ui)
        (loop for pathname in (user-message-input-image-pathnames input)
              for number from 1
              collect (cons (terminal-ui--image-label number) pathname)))
  (line-editor-set-text
   (terminal-ui-editor ui)
   (sanitize-text (user-message-input-text input)))
  nil)

(-> terminal-ui-live-row-count (terminal-ui) (integer 0))
(defun terminal-ui-live-row-count (ui)
  "Return the number of live physical rows currently painted for UI."
  (live-region-row-count (terminal-ui-live-region ui)))

(-> terminal--write-newline (terminal) null)
(defun terminal--write-newline (terminal)
  "Write a line break that returns to column zero on interactive terminals."
  (terminal--write terminal
                   (if (terminal-interactive-p terminal)
                       (format nil "~C~C" #\Return #\Newline)
                       (string #\Newline)))
  nil)

(-> terminal--write-safe-text (terminal string) null)
(defun terminal--write-safe-text (terminal text)
  "Write trusted TEXT while making its line endings terminal-safe."
  (let ((line-start 0))
    (loop for newline = (position #\Newline text :start line-start)
          while newline
          do (terminal--write terminal (subseq text line-start newline))
             (terminal--write-newline terminal)
             (setf line-start (1+ newline))
          finally (terminal--write terminal (subseq text line-start))))
  nil)




(-> terminal--spans-text (list) string)
(defun terminal--spans-text (spans)
  "Return sanitized visible text for semantic SPANS."
  (termdown:spans-text spans))





(-> terminal--render-spans (terminal list) string)
(defun terminal--render-spans (terminal spans)
  "Render semantic SPANS using the application's style table."
  (termdown:render-spans
   spans :style-function
   (when (terminal-styled-p terminal)
     (lambda (role text)
       (let ((sequence (terminal-style-sequence role)))
         (if sequence
             (concatenate 'string sequence text *terminal-style-reset*)
             text))))))


(-> terminal-ui--lisp-draft-p (string) boolean)
(defun terminal-ui--lisp-draft-p (text)
  "Return true when TEXT begins with explicit Common Lisp input."
  (and (plusp (length text))
       (char= #\( (char text 0))))

(-> terminal-ui--prompt-spans (string boolean) (values string list))
(defun terminal-ui--prompt-spans (prompt lisp-draft-p)
  "Return PROMPT's visible text and styled spans for the current input mode."
  (if lisp-draft-p
      (let ((marker
              (position-if-not
               (lambda (character)
                 (find character '(#\Space #\Tab #\Newline #\Return #\Page)))
               prompt
               :from-end t)))
        (if marker
            (let ((text (copy-seq prompt)))
              (setf (char text marker) #\*)
              (values
               text
               (append
                (when (plusp marker)
                  (list (terminal-span ':brand (subseq text 0 marker))))
                (list (terminal-span ':lisp-prompt "*"))
                (when (< (1+ marker) (length text))
                  (list (terminal-span ':brand (subseq text (1+ marker))))))))
            (values "* "
                    (list (terminal-span ':lisp-prompt "*")
                          (terminal-span ':brand " ")))))
      (values prompt (list (terminal-span ':brand prompt)))))

(-> terminal-ui--prompt-content
    (terminal-ui)
    (values (or list terminal-rendered-row) integer))
(defun terminal-ui--prompt-content (ui)
  "Return UI's word-wrapped prompt content and cursor character offset.

The rendered row is memoized on its exact inputs, because most repaints
run while the draft, cursor, and terminal width are unchanged. The editor
installs a fresh string on every text change, so identity comparison on
the draft is exact."
  (let* ((editor       (terminal-ui-editor ui))
         (raw-content  (line-editor-text editor))
         (lisp-draft-p
           (not (null (or (terminal-ui-lisp-input-p ui)
                          (terminal-ui--lisp-draft-p raw-content)))))
         (key
           (list raw-content
                 (line-editor-cursor editor)
                 (terminal-columns (terminal-ui-terminal ui))
                 lisp-draft-p
                 (terminal-ui-prompt ui)
                 (terminal-ui-placeholder ui)
                 *terminal-style-table*
                 *terminal-style-reset*))
         (cache        (terminal-ui-prompt-render-cache ui)))
    (if (and cache (every #'eql (first cache) key))
        (values (second cache) (third cache))
        (multiple-value-bind (content cursor-offset)
            (terminal-ui--render-prompt-content ui lisp-draft-p)
          (setf (terminal-ui-prompt-render-cache ui)
                (list key content cursor-offset))
          (values content cursor-offset)))))

(-> terminal-ui--render-prompt-content
    (terminal-ui boolean)
    (values (or list terminal-rendered-row) integer))
(defun terminal-ui--render-prompt-content (ui lisp-draft-p)
  "Render UI's word-wrapped prompt content and cursor character offset."
  (let* ((terminal (terminal-ui-terminal ui))
         (columns (terminal-columns terminal))
         (editor (terminal-ui-editor ui))
         (raw-content (line-editor-text editor))
         (safe-prompt (sanitize-text (terminal-ui-prompt ui)
                                     :single-line-p t)))
    (multiple-value-bind (prompt-text prompt-spans)
        (terminal-ui--prompt-spans safe-prompt lisp-draft-p)
      (if (and (zerop (length raw-content))
               (non-empty-string-p (terminal-ui-placeholder ui)))
          (let ((spans
                  (terminal--clip-spans
                   (append prompt-spans
                           (list
                            (terminal-span :hint
                                           (terminal-ui-placeholder ui))))
                   columns)))
            (values spans
                    (min (length prompt-text)
                         (length (terminal--spans-text spans)))))
          (let* ((content (sanitize-text raw-content))
                 (content-cursor
                   (length
                    (sanitize-text
                     (subseq raw-content 0 (line-editor-cursor editor)))))
                 (content-spans
                   (or
                    (and lisp-draft-p
                         (syntax--highlight-spans
                          content
                          :language (language-find ':common-lisp :errorp nil)))
                    (list
                     (terminal-span
                      (if (uiop:string-prefix-p "/" content)
                          ':user
                          ':plain)
                      content))))
                 (prompt-display
                   (terminal--render-spans terminal prompt-spans))
                 (content-display
                   (terminal--render-spans terminal content-spans)))
            (multiple-value-bind
                  (wrapped-content wrapped-display wrapped-cursor)
                (wrap-styled-editor-text
                 content
                 content-display
                 :cursor content-cursor
                 :columns columns
                 :prompt-width (text-cell-width prompt-text))
              (values
               (terminal--make-rendered-row
                (concatenate 'string prompt-text wrapped-content)
                (concatenate 'string prompt-display wrapped-display))
               (+ (length prompt-text) wrapped-cursor))))))))

;;;; -- Operation Completion Suggestions --

(-> terminal-ui--current-completions (terminal-ui) list)
(defun terminal-ui--current-completions (ui)
  "Return and validate UI's current static or dynamically provided completions."
  (let ((completions
          (if (terminal-ui-completion-function ui)
              (funcall (terminal-ui-completion-function ui))
              (terminal-ui-completions ui))))
    (unless (and (listp completions)
                 (every #'terminal-completion-p completions))
      (error 'terminal-error
             :message "The completion provider returned invalid entries."
             :operation ':complete
             :cause nil))
    completions))

(-> terminal-ui--operation-completion-prefix-p (string) boolean)
(defun terminal-ui--operation-completion-prefix-p (text)
  "Return whether TEXT can prefix a slash or parenthesized completion."
  (and (non-empty-string-p text)
       (member (char text 0) '(#\/ #\() :test #'char=)
       (not (find-if (lambda (character)
                       (find character '(#\Newline #\Return)))
                     text))
       (or (char= (char text 0) #\/)
           (not (find #\) text)))))

(-> terminal-ui--matching-completions (terminal-ui) list)
(defun terminal-ui--matching-completions (ui)
  "Return registered operation completions extending the current name prefix."
  (let ((text (line-editor-text (terminal-ui-editor ui)))
        (completions (terminal-ui--current-completions ui)))
    (if (and (terminal-interactive-p (terminal-ui-terminal ui))
             completions
             (terminal-ui--operation-completion-prefix-p text))
        (remove-if-not
         (lambda (entry)
           (uiop:string-prefix-p (string-downcase text)
                                 (string-downcase (getf entry :name))))
         completions)
        nil)))

(-> terminal-ui--reconcile-completions (terminal-ui) list)
(defun terminal-ui--reconcile-completions (ui)
  "Return UI's current matches, resetting the selection when the set changes."
  (let ((selector (terminal-ui-completion-selector ui)))
    (if (terminal-ui-completion-active-p ui)
        (selector-items selector)
        (let ((matches (terminal-ui--matching-completions ui)))
          (selector-set-items selector matches)
          matches))))

(-> terminal-completion-label (list) string)
(defun terminal-completion-label (entry)
  "Return completion ENTRY's display name including its argument hint."
  (let ((argument (getf entry :argument)))
    (if argument
        (format nil "~A ~A" (getf entry :name) argument)
        (getf entry :name))))

(-> terminal-ui--picker-default-search-text (list) string)
(defun terminal-ui--picker-default-search-text (entry)
  "Return the searchable visible metadata carried by picker ENTRY."
  (format nil
          "~{~A~^ ~}"
          (loop for key in '(:name :argument :group :tally :description)
                for value = (getf entry key)
                when (stringp value)
                  collect value)))

(-> terminal-ui--picker-search-terms (string) list)
(defun terminal-ui--picker-search-terms (query)
  "Return QUERY's non-empty whitespace-separated search terms."
  (remove-if (lambda (term) (zerop (length term)))
             (uiop:split-string query
                                :separator '(#\Space #\Tab #\Newline #\Return))))

(-> terminal-ui--picker-search-match-p (list string function) boolean)
(defun terminal-ui--picker-search-match-p (entry query search-key)
  "Return true when every term in QUERY occurs in ENTRY's SEARCH-KEY text."
  (let ((text (funcall search-key entry)))
    (unless (stringp text)
      (error 'terminal-error
             :message "A picker search key must return a string."
             :operation ':select
             :cause nil))
    (not
     (null
      (every (lambda (term)
               (search term text :test #'char-equal))
             (terminal-ui--picker-search-terms query))))))

(-> terminal-ui--picker-search-title (string string integer) string)
(defun terminal-ui--picker-search-title (title query match-count)
  "Return TITLE annotated with QUERY and MATCH-COUNT when search is active."
  (if (plusp (length query))
      (format nil "~A · search: ~A · ~D match~:P"
              title query match-count)
      title))

(-> terminal-ui--choice-tally (list) string)
(defun terminal-ui--choice-tally (entry)
  "Return ENTRY's optional tally column text."
  (let ((tally (getf entry :tally)))
    (if (stringp tally)
        tally
        "")))


(-> terminal-ui--choice-description-spans (list boolean integer) list)
(defun terminal-ui--choice-description-spans (entry selected-p maximum-width)
  "Return ENTRY's selection-aware description spans clipped to MAXIMUM-WIDTH."
  (let ((default-style (if selected-p ':plain ':dim)))
    (terminal--clip-spans
     (loop for span in (or (getf entry :description-spans)
                           (list (terminal-span ':plain
                                                (getf entry :description))))
           collect
           (terminal-span
            (if (eq (terminal-span-style span) ':plain)
                default-style
                (terminal-span-style span))
            (terminal-span-text span)))
     maximum-width)))


(-> terminal-ui--choice-rows (selector integer) list)
(defun terminal-ui--choice-rows (selector row-width)
  "Return styled candidate rows and nonselectable group headings.

When any candidate carries a non-empty :TALLY string, rows render three
columns: name, tally, and description."
  (multiple-value-bind (index-rows arrangement-widths)
      (selector-arrange selector
                        row-width
                        :width-function
                        (lambda (entry)
                          (text-cell-width
                           (terminal-completion-label entry))))
    (declare (ignore arrangement-widths))
    (let* ((tally-p
             (loop for entry in (selector-items selector)
                   thereis (plusp (length (terminal-ui--choice-tally entry)))))
           (cell-rows
             (loop for entry in (selector-items selector)
                   collect
                   (if tally-p
                       (list (terminal-completion-label entry)
                             (terminal-ui--choice-tally entry)
                             (or (getf entry :description) ""))
                       (list (terminal-completion-label entry)
                             (or (getf entry :description) "")))))
           (column-widths
             (layout-column-widths cell-rows
                                   (max 0 (- row-width 2))
                                   :gap-width 2
                                   :minimum-widths
                                   (if tally-p
                                       '(1 0 0)
                                       '(1 0))))
           (label-width (or (first column-widths) 0))
           (tally-width (if tally-p (or (second column-widths) 0) 0))
           (description-width
             (if tally-p
                 (or (third column-widths) 0)
                 (or (second column-widths) 0))))
      (let ((previous-group nil))
        (loop for index-row in index-rows
              for index = (first index-row)
              for entry = (nth index (selector-items selector))
              for selected-p = (= index (selector-selection selector))
              for group = (getf entry :group)
              append
              (prog1
                  (append
                   (when (and group (not (equal group previous-group)))
                     (append
                      (when previous-group (list nil))
                      (list
                       (terminal--clip-spans
                        (list (terminal-span ':strong
                                             (format nil "  ~A" group)))
                        row-width))))
                   (list
                    (terminal--clip-spans
                     (append
                      (list (terminal-span (if selected-p
                                               :brand
                                               :dim)
                                           (if selected-p
                                               "▸ "
                                               "  "))
                            (terminal-span :user
                                           (layout-fit-text
                                            (terminal-completion-label entry)
                                            label-width)))
                      (when tally-p
                        (list
                         (terminal-span ':plain
                                        (if (plusp tally-width)
                                            "  "
                                            ""))
                         (terminal-span
                          (if selected-p ':plain ':dim)
                          (layout-fit-text
                           (terminal-ui--choice-tally entry)
                           tally-width))))
                      (append
                       (list
                        (terminal-span ':plain
                                       (if (plusp description-width)
                                           "  "
                                           "")))
                       (terminal-ui--choice-description-spans
                        entry selected-p description-width)))
                     row-width)))
                (setf previous-group group)))))))

(-> terminal-ui--editor-history-navigating-p (line-editor) boolean)
(defun terminal-ui--editor-history-navigating-p (editor)
  "Return true when EDITOR is currently traversing history."
  (and (slot-boundp editor 'clinedi::history-index)
       (not (null (slot-value editor 'clinedi::history-index)))
       t))


(-> terminal-ui--snapshot-history-state (line-editor) (option list))
(defun terminal-ui--snapshot-history-state (editor)
  "Return EDITOR's history traversal state, or NIL outside history recall."
  (when (terminal-ui--editor-history-navigating-p editor)
    (list (copy-seq (line-editor-text editor))
          (line-editor-cursor editor)
          (slot-value editor 'clinedi::history-index)
          (copy-seq (slot-value editor 'clinedi::history-stash))
          (slot-value editor 'clinedi::history-stash-cursor))))


(-> terminal-ui--restore-history-state (line-editor list) null)
(defun terminal-ui--restore-history-state (editor state)
  "Restore EDITOR's history traversal STATE after completion is cancelled."
  (destructuring-bind (text cursor index stash stash-cursor) state
    (line-editor-set-text editor text :cursor cursor)
    (setf (slot-value editor 'clinedi::history-index) index
          (slot-value editor 'clinedi::history-stash) stash
          (slot-value editor 'clinedi::history-stash-cursor) stash-cursor))
  nil)

(-> terminal-ui--completion-offered-p (terminal-ui) boolean)
(defun terminal-ui--completion-offered-p (ui)
  "Return true when UI may paint or begin command completion."
  (or (terminal-ui-completion-active-p ui)
      (and (not (terminal-ui-completion-dismissed-p ui))
           (not (terminal-ui--editor-history-navigating-p
                 (terminal-ui-editor ui))))))


(-> terminal-ui-completion-menu-present-p (terminal-ui) boolean)
(defun terminal-ui-completion-menu-present-p (ui)
  "Return true when UI currently renders command completion candidates."
  (and (terminal-ui--completion-offered-p ui)
       (not
        (null
         (selector-items (terminal-ui-completion-selector ui))))))


(-> terminal-ui--completion-rows (terminal-ui integer) list)
(defun terminal-ui--completion-rows (ui row-width)
  "Return styled rows for UI's matching command completions."
  (block nil
    (unless (terminal-ui--completion-offered-p ui)
      (selector-set-items (terminal-ui-completion-selector ui) nil)
      (return nil))
    (terminal-ui--reconcile-completions ui)
    (terminal-ui--choice-rows (terminal-ui-completion-selector ui) row-width)))

(-> terminal-ui--accept-completion (terminal-ui list) null)
(defun terminal-ui--accept-completion (ui entry)
  "Replace UI's input with ENTRY's name, adding a space when it takes an argument."
  (line-editor-set-text
   (terminal-ui-editor ui)
   (sanitize-text
    (concatenate 'string
                 (getf entry :name)
                 (if (getf entry :argument)
                     " "
                     ""))))
  nil)

(-> terminal-ui--begin-completion (terminal-ui) null)
(defun terminal-ui--begin-completion (ui)
  "Begin choosing among UI's current command completion candidates."
  (unless (terminal-ui-completion-active-p ui)
    (let ((editor (terminal-ui-editor ui)))
      (setf (terminal-ui-completion-prefix ui)
            (line-editor-text editor)
            (terminal-ui-completion-history-state ui)
            (terminal-ui--snapshot-history-state editor)
            (terminal-ui-completion-dismissed-p ui) nil
            (terminal-ui-completion-active-p ui) t)))
  nil)

(-> terminal-ui--end-completion (terminal-ui) null)
(defun terminal-ui--end-completion (ui)
  "Leave UI's active command completion selection without changing input."
  (setf (terminal-ui-completion-active-p ui) nil
        (terminal-ui-completion-prefix ui) nil
        (terminal-ui-completion-history-state ui) nil)
  nil)

(-> terminal-ui--cancel-completion (terminal-ui) null)
(defun terminal-ui--cancel-completion (ui)
  "Cancel completion and restore its original input or history traversal state."
  (let ((prefix (terminal-ui-completion-prefix ui))
        (history-state (terminal-ui-completion-history-state ui))
        (editor (terminal-ui-editor ui)))
    (cond
      (history-state
       (terminal-ui--restore-history-state editor history-state))
      (prefix
       (line-editor-set-text editor prefix))))
  (selector-set-items (terminal-ui-completion-selector ui) nil)
  (terminal-ui--end-completion ui)
  (setf (terminal-ui-completion-dismissed-p ui) t)
  nil)

(-> terminal-ui--handle-completion-event
    (terminal-ui t)
    (values (option keyword) (option string)))
(defun terminal-ui--handle-completion-event (ui event)
  "Apply EVENT to UI's completion suggestions and return its action when consumed."
  (block nil
    (unless (or (terminal-ui--completion-offered-p ui)
                (member event '(:complete :complete-previous)))
      (return (values nil nil)))
    (terminal-ui--reconcile-completions ui)
    (let ((selector (terminal-ui-completion-selector ui)))
      (unless (selector-items selector)
        (return (values nil nil)))
        (unless (or (terminal-ui-completion-active-p ui)
                    (member event
                            '(:up :down :complete :complete-previous
                              :submit :escape)))
          (return (values nil nil)))
      (when (member event '(:up :down :complete :complete-previous))
        (terminal-ui--begin-completion ui))
      (multiple-value-bind (selector-action entry)
          (selector-handle-event selector event)
        (case selector-action
          (:changed
           (terminal-ui--accept-completion ui entry)
           (terminal-ui--repaint-live ui)
           (values :changed nil))
          (:accept
           (terminal-ui--end-completion ui)
           (terminal-ui--accept-completion ui entry)
           (cond
             ((getf entry :argument)
              (terminal-ui--repaint-live ui)
              (values :changed nil))
             (t
              (multiple-value-bind (action payload)
                  (line-editor-handle-event (terminal-ui-editor ui) :submit)
                (terminal-ui--repaint-live ui)
                (values action payload)))))
          (:cancel
           (terminal-ui--cancel-completion ui)
           (terminal-ui--repaint-live ui)
           (values :changed nil))
          (:dismiss
           (terminal-ui--end-completion ui)
           (values nil nil))
          (t
           (values nil nil)))))))

(-> terminal-ui--pending-input-rows
    (string list &key (:count integer) (:row-width integer))
    list)
(defun terminal-ui--pending-input-rows
    (label inputs &key count row-width)
  "Return bounded live rows previewing pending INPUTS under LABEL."
  (let* ((visible-count (min *terminal-ui-pending-preview-limit*
                             (length inputs)))
         (omitted (- count visible-count)))
    (append
     (loop for input in (subseq inputs 0 visible-count)
           for index from 1
           collect
           (terminal--clip-spans
            (list (terminal-span ':brand "∙ ")
                  (terminal-span ':dim
                                 (format nil "~A ~D/~D  "
                                         label index count))
                  (terminal-span
                   ':plain
                   (sanitize-text input :single-line-p t)))
            row-width))
     (when (plusp omitted)
       (list
        (terminal--clip-spans
         (list (terminal-span ':brand "∙ ")
               (terminal-span ':dim
                              (format nil "~D more ~A input~:P"
                                      omitted label)))
         row-width))))))


;;;; -- Live Region Composition --

(-> terminal-ui--agent-spinner-phase-at (real) (integer 0))
(defun terminal-ui--agent-spinner-phase-at (now)
  "Return the shared child-agent spinner phase at monotonic NOW."
  (mod (floor (* *terminal-ui-status-spinner-frames-per-second*
                 (max 0 now)))
       (length *terminal-ui-agent-spinner-frames*)))

(-> terminal-ui--duration-text (integer) string)
(defun terminal-ui--duration-text (seconds)
  "Format non-negative SECONDS as a compact activity duration."
  (let ((seconds (max 0 seconds)))
    (multiple-value-bind (minutes remaining-seconds)
        (floor seconds 60)
      (multiple-value-bind (hours remaining-minutes)
          (floor minutes 60)
        (if (plusp hours)
            (format nil "~D:~2,'0D:~2,'0D"
                    hours remaining-minutes remaining-seconds)
            (format nil "~2,'0D:~2,'0D"
                    remaining-minutes remaining-seconds))))))

(-> terminal-ui--agent-trace-tools
    (list (integer 1))
    (values list (integer 0)))
(defun terminal-ui--agent-trace-tools (activity limit)
  "Return ACTIVITY's newest LIMIT tool milestones and omitted count."
  (let* ((current-tool (getf activity :current-tool))
         (tools
           (append (copy-list (getf activity :recent-tools))
                   (when (non-empty-string-p current-tool)
                     (list current-tool))))
         (omitted-count (max 0 (- (length tools) limit))))
    (values (nthcdr omitted-count tools) omitted-count)))

(-> terminal-ui--activity-duration-span-at (list real) (option list))
(defun terminal-ui--activity-duration-span-at (activity now)
  "Return ACTIVITY's current duration span at monotonic NOW."
  (let* ((duration-ms
           (or (and (getf activity :current-tool)
                    (getf activity :current-tool-duration-ms))
               (getf activity :duration-ms)))
         (observed-at (getf activity :observed-at)))
    (and duration-ms
         observed-at
         (terminal-span
          ':timestamp-time
          (format nil " ~A"
                  (terminal-ui--duration-text
                   (floor
                    (+ (/ duration-ms 1000.0d0)
                       (max 0 (- now observed-at))))))))))

(-> terminal-ui--agent-trace-spans-at
    (list real (integer 1) &key (:show-omitted-p boolean))
    list)
(defun terminal-ui--agent-trace-spans-at
    (activity now limit &key (show-omitted-p t))
  "Return a bounded chronological tool trace for ACTIVITY at monotonic NOW."
  (multiple-value-bind (tools omitted-count)
      (terminal-ui--agent-trace-tools activity limit)
    (let* ((current-tool (getf activity :current-tool))
           (tool-count (length tools))
           (duration-span
             (terminal-ui--activity-duration-span-at activity now)))
      (if tools
          (append
           (when (and show-omitted-p (plusp omitted-count))
             (list (terminal-span ':dim "… › ")))
           (loop for tool in tools
                 for index from 0
                 append
                 (append
                  (unless (zerop index)
                    (list (terminal-span ':dim " › ")))
                  (list
                   (terminal-span
                    (if (and current-tool (= index (1- tool-count)))
                        ':agent-tool
                        ':dim)
                    tool))))
           (when duration-span (list duration-span)))
          (append
           (list
            (terminal-span
             ':dim
             (if (plusp (getf activity :request-count))
                 (format nil "request ~D" (getf activity :request-count))
                 "starting")))
           (when duration-span (list duration-span)))))))

(-> terminal-ui--agent-primary-detail-spans-at (list real) list)
(defun terminal-ui--agent-primary-detail-spans-at (activity now)
  "Return compact status and trace spans for ACTIVITY's primary row."
  (let ((running-p (eq (getf activity :state) ':running))
        (blocking-p (not (getf activity :detached))))
    (cond
      ((not running-p)
       (append
        (when blocking-p
          (list (terminal-span ':strong "blocking")
                (terminal-span ':dim " · ")))
        (list (terminal-span ':dim "queued"))))
      (blocking-p
       (append
        (list (terminal-span ':strong "blocking")
              (terminal-span ':dim " · "))
        (terminal-ui--agent-trace-spans-at
         activity now 1 :show-omitted-p nil)))
      (t
       (terminal-ui--agent-trace-spans-at
        activity now *terminal-ui-agent-compact-trace-limit*)))))

(-> terminal-ui--agent-activity-row-at
    (list real integer &key (:identity-width integer) (:role-width integer))
    list)
(defun terminal-ui--agent-activity-row-at
    (activity now row-width &key (identity-width 0) (role-width 0))
  "Return one clipped primary child ACTIVITY row at monotonic NOW."
  (let* ((running-p (eq (getf activity :state) ':running))
         (detail-spans
           (terminal-ui--agent-primary-detail-spans-at activity now)))
    (terminal--clip-spans
     (append
      (list
       (terminal-span ':dim "  ")
       (if running-p
           (terminal-span
            ':agent-spinner
            (format nil "~A "
                    (aref *terminal-ui-agent-spinner-frames*
                          (terminal-ui--agent-spinner-phase-at now))))
           (terminal-span ':dim "○ "))
       (terminal-span ':agent-name
                      (layout-fit-text (getf activity :id)
                                       identity-width))
       (terminal-span ':dim " · ")
       (terminal-span ':agent-role
                      (layout-fit-text (getf activity :agent)
                                       role-width)))
      (when detail-spans
        (cons (terminal-span ':dim " · ") detail-spans)))
     row-width)))

(-> terminal-ui--agent-expanded-row-at (list real integer) (option list))
(defun terminal-ui--agent-expanded-row-at (activity now row-width)
  "Return the extra bounded trace row shown for one blocking ACTIVITY."
  (unless (getf activity :detached)
    (let* ((running-p (eq (getf activity :state) ':running))
           (assignment (getf activity :assignment))
           (trace-spans
             (and running-p
                  (terminal-ui--agent-trace-spans-at
                   activity now *terminal-ui-agent-expanded-trace-limit*))))
      (when (or trace-spans (non-empty-string-p assignment))
        (terminal--clip-spans
         (append
          (list (terminal-span ':dim "      ")
                (terminal-span ':brand "↳ "))
          trace-spans
          (when (and trace-spans (non-empty-string-p assignment))
            (list (terminal-span ':dim " · ")))
          (when (non-empty-string-p assignment)
            (list (terminal-span ':dim assignment))))
         row-width)))))

(-> terminal-ui--command-activity-row-at
    (list real integer &key (:identity-width integer) (:tool-width integer))
    list)
(defun terminal-ui--command-activity-row-at
    (activity now row-width &key (identity-width 0) (tool-width 0))
  "Return one clipped primary command ACTIVITY row at monotonic NOW."
  (let* ((running-p (eq (getf activity :state) ':running))
         (state-spans
           (if running-p
               (let ((duration
                       (terminal-ui--activity-duration-span-at activity now)))
                 (and duration (list duration)))
               (list (terminal-span ':dim " · queued"))))
         (prefix
           (list
            (terminal-span ':dim "  ")
            (if running-p
                (terminal-span
                 ':command-spinner
                 (format nil "~A "
                         (aref *terminal-ui-agent-spinner-frames*
                               (terminal-ui--agent-spinner-phase-at now))))
                (terminal-span ':dim "○ "))
            (terminal-span
             ':command-id
             (layout-fit-text (getf activity :id) identity-width))
            (terminal-span ':dim " · ")
            (terminal-span
             ':command-tool
             (layout-fit-text (getf activity :tool) tool-width))
            (terminal-span ':dim " · ")))
         (description-width
           (max 0
                (- row-width
                   (terminal--spans-width prefix)
                   (terminal--spans-width state-spans)))))
    (terminal--clip-spans
     (append
      prefix
      (list
       (terminal-span
        ':plain
        (layout-fit-text (getf activity :description) description-width)))
      state-spans)
     row-width)))

(-> terminal-ui--command-presentation-activities (terminal-ui) list)
(defun terminal-ui--command-presentation-activities (ui)
  "Return current and not-yet-painted completed command rows in job order."
  (let ((current (terminal-ui-command-activities ui)))
    (sort
     (append
      (copy-list current)
      (remove-if
       (lambda (pending)
         (find (getf pending :id)
               current
               :key (lambda (activity)
                      (getf activity :id))
               :test #'string=))
       (copy-list (terminal-ui-command-pending-completions ui))))
     #'<
     :key (lambda (activity)
            (getf activity :index)))))

(-> terminal-ui--animated-status-row-p (terminal-ui) boolean)
(defun terminal-ui--animated-status-row-p (ui)
  "Return whether UI has live activity needing an animated status row."
  (not
   (null
    (or (terminal-ui-status ui)
        (terminal-ui-local-activity ui)
        (terminal-ui-compacting-p ui)
        (terminal-ui--command-presentation-activities ui)
        (terminal-ui-agent-activities ui)
        (terminal-ui-preview-rows ui)
        (terminal-ui-stream-tail ui)))))

(-> terminal-ui--status-row-visible-p (terminal-ui) boolean)
(defun terminal-ui--status-row-visible-p (ui)
  "Return whether UI needs its static context/details or animated status row."
  (not
   (null
    (or (terminal-ui--animated-status-row-p ui)
        (terminal-ui-idle-status-details ui)
        (terminal-ui-context-window ui)))))

(-> terminal-ui--activity-row-budget (terminal-ui) (integer 1))
(defun terminal-ui--activity-row-budget (ui)
  "Return the viewport rows available to UI's command and child activity strips."
  (let* ((status-p         (not (null (terminal-ui-status ui))))
         (local-activity-p (not (null (terminal-ui-local-activity ui))))
         (compacting-p     (terminal-ui-compacting-p ui))
         (status-row-p     (terminal-ui--status-row-visible-p ui))
         (reserved-rows
           (+ 3
              (if status-row-p 2 0)
              (if compacting-p 1 0)
              (if local-activity-p 1 0)
              (if status-p 1 0))))
    (max 1
         (- (terminal-ui--maximum-live-rows (terminal-ui-terminal ui))
            reserved-rows))))

(-> terminal-ui--command-row-demand (terminal-ui) (integer 0))
(defun terminal-ui--command-row-demand (ui)
  "Return the useful maximum row demand for UI's command activity strip."
  (let ((count
          (length (terminal-ui--command-presentation-activities ui))))
    (if (plusp count)
        (+ 2 (min count *terminal-ui-command-visible-limit*))
        0)))

(-> terminal-ui--activity-row-budgets
    (terminal-ui)
    (values (integer 0) (integer 0)))
(defun terminal-ui--activity-row-budgets (ui)
  "Return fair command and child row budgets within UI's shared viewport bound."
  (let ((total (terminal-ui--activity-row-budget ui))
        (commands-p
          (not
           (null (terminal-ui--command-presentation-activities ui))))
        (agents-p (not (null (terminal-ui-agent-activities ui)))))
    (cond
      ((and commands-p agents-p)
       (let ((command-budget
               (min (terminal-ui--command-row-demand ui)
                    (max 1 (1- total)))))
         (values command-budget (- total command-budget))))
      (commands-p
       (values total 0))
      (agents-p
       (values 0 total))
      (t
       (values 0 0)))))

(-> terminal-ui--command-row-budget (terminal-ui) (integer 0))
(defun terminal-ui--command-row-budget (ui)
  "Return UI's allocated command activity rows."
  (terminal-ui--activity-row-budgets ui))

(-> terminal-ui--agent-row-budget (terminal-ui) (integer 0))
(defun terminal-ui--agent-row-budget (ui)
  "Return UI's allocated child-agent activity rows."
  (nth-value 1 (terminal-ui--activity-row-budgets ui)))

(-> terminal-ui--command-activity-selection
    (terminal-ui)
    (values list list (integer 0) boolean (integer 0)))
(defun terminal-ui--command-activity-selection (ui)
  "Return all, visible, header, overflow, and omission command row values."
  (let* ((activities (terminal-ui--command-presentation-activities ui))
         (row-budget (terminal-ui--command-row-budget ui))
         (header-row-count
           (cond
             ((zerop row-budget) 0)
             ((= row-budget 1) 0)
             ((< row-budget 4) 1)
             (t 2)))
         (body-row-budget (max 0 (- row-budget header-row-count)))
         (limited-visible-count
           (min *terminal-ui-command-visible-limit*
                (length activities)
                body-row-budget))
         (overflow-row-p
           (and (> (length activities) limited-visible-count)
                (> body-row-budget 1)))
         (visible-count
           (if overflow-row-p
               (min *terminal-ui-command-visible-limit*
                    (length activities)
                    (1- body-row-budget))
               limited-visible-count)))
    (values activities
            (subseq activities 0 visible-count)
            header-row-count
            (not (null overflow-row-p))
            (- (length activities) visible-count))))

(-> terminal-ui--command-visible-activities (terminal-ui) list)
(defun terminal-ui--command-visible-activities (ui)
  "Return the command activities represented by rows in UI's next paint."
  (nth-value 1 (terminal-ui--command-activity-selection ui)))

(-> terminal-ui--command-activity-rows-at
    (terminal-ui real integer)
    list)
(defun terminal-ui--command-activity-rows-at (ui now row-width)
  "Return viewport-bounded primary command rows for UI at monotonic NOW."
  (multiple-value-bind
      (activities visible-activities header-row-count overflow-row-p omitted-count)
      (terminal-ui--command-activity-selection ui)
    (let* ((column-widths
             (layout-column-widths
              (loop for activity in visible-activities
                    collect
                    (list (getf activity :id)
                          (getf activity :tool)))
              (max 0
                   (- row-width
                      (text-cell-width "  ○  ·  ·  00:00")
                      12))
              :gap-width 3
              :minimum-widths '(1 1)))
           (identity-width (or (first column-widths) 0))
           (tool-width (or (second column-widths) 0))
           (header-row
             (terminal--clip-spans
              (list (terminal-span ':tool "commands")
                    (terminal-span ':dim
                                   (format nil " ~D" (length activities))))
              row-width)))
      (when (and activities
                 (plusp (terminal-ui--command-row-budget ui)))
        (append
         (when (plusp header-row-count)
           (append (list header-row)
                   (when (= header-row-count 2)
                     (list nil))))
         (loop for activity in visible-activities
               collect
               (terminal-ui--command-activity-row-at
                activity now row-width
                :identity-width identity-width
                :tool-width tool-width))
         (when overflow-row-p
           (list
            (terminal--clip-spans
             (list (terminal-span ':dim "  ")
                   (terminal-span
                    ':dim
                    (format nil "… ~D more command~:P" omitted-count)))
             row-width))))))))


(-> terminal-ui--agent-activity-rows-at
    (terminal-ui real integer)
    list)
(defun terminal-ui--agent-activity-rows-at (ui now row-width)
  "Return viewport-bounded child-agent rows for UI at monotonic NOW."
  (let* ((activities (terminal-ui-agent-activities ui))
         (row-budget (terminal-ui--agent-row-budget ui))
         (header-row-count
           (cond
             ((zerop row-budget) 0)
             ((= row-budget 1) 0)
             ((< row-budget 4) 1)
             (t 2)))
         (body-row-budget (max 0 (- row-budget header-row-count)))
         (limited-visible-count
           (min *terminal-ui-agent-visible-limit*
                (length activities)
                body-row-budget))
         (overflow-row-p
           (and (> (length activities) limited-visible-count)
                (> body-row-budget 1)))
         (visible-count
           (if overflow-row-p
               (min *terminal-ui-agent-visible-limit*
                    (length activities)
                    (1- body-row-budget))
               limited-visible-count))
         (omitted-count (- (length activities) visible-count))
         (visible-activities (subseq activities 0 visible-count))
         (base-row-count
           (+ header-row-count
              visible-count
              (if overflow-row-p 1 0)))
         (expanded-row-limit (max 0 (- row-budget base-row-count)))
         (blocking-activities
           (remove-if (lambda (activity)
                        (getf activity :detached))
                      visible-activities))
         (running-blocking-activities
           (remove-if-not (lambda (activity)
                            (eq (getf activity :state) ':running))
                          blocking-activities))
         (queued-blocking-activities
           (remove-if (lambda (activity)
                        (eq (getf activity :state) ':running))
                      blocking-activities))
         (expandable-activities
           (append running-blocking-activities queued-blocking-activities))
         (expanded-activities
           (subseq expandable-activities
                   0
                   (min expanded-row-limit
                        (length expandable-activities))))
         (column-widths
           (layout-column-widths
            (loop for activity in visible-activities
                  collect
                  (list (getf activity :id)
                        (getf activity :agent)))
            (max 0 (- row-width (text-cell-width "  ○ ")))
            :gap-width 3
            :minimum-widths '(1 1)))
         (identity-width (or (first column-widths) 0))
         (role-width (or (second column-widths) 0))
         (header-row
           (terminal--clip-spans
            (list (terminal-span ':agent-name "agents")
                  (terminal-span ':dim
                                 (format nil " ~D" (length activities))))
            row-width)))
    (when (and activities (plusp row-budget))
      (append
       (when (plusp header-row-count)
         (append (list header-row)
                 (when (= header-row-count 2)
                   (list nil))))
       (loop for activity in visible-activities
             append
             (append
              (list
               (terminal-ui--agent-activity-row-at
                activity now row-width
                :identity-width identity-width
                :role-width role-width))
              (when (member activity expanded-activities :test #'eq)
                (let ((expanded-row
                        (terminal-ui--agent-expanded-row-at
                         activity now row-width)))
                  (when expanded-row
                    (list expanded-row))))))
       (when overflow-row-p
         (list
          (terminal--clip-spans
           (list (terminal-span ':dim "  ")
                 (terminal-span ':dim
                                (format nil "… ~D more agent~:P"
                                        omitted-count)))
           row-width)))))))

(-> terminal-ui--status-timing-at
    (terminal-ui real)
    (values real real))
(defun terminal-ui--status-timing-at (ui now)
  "Return UI's active status start and newest progress times at NOW."
  (cond
    ((terminal-ui-status ui)
     (let ((started-at (or (terminal-ui-status-started-at ui) now)))
       (values started-at
               (or (terminal-ui-status-progress-at ui) started-at))))
    ((terminal-ui-local-activity ui)
     (let ((started-at (or (terminal-ui-local-activity-started-at ui) now)))
       (values started-at started-at)))
    ((terminal-ui-compacting-p ui)
     (let ((started-at (or (terminal-ui-compaction-started-at ui) now)))
       (values started-at started-at)))
    ((or (terminal-ui--command-presentation-activities ui)
         (terminal-ui-agent-activities ui))
     (let* ((activities
              (append (terminal-ui--command-presentation-activities ui)
                      (terminal-ui-agent-activities ui)))
            (started-at
              (or (loop for activity in activities
                        minimize (getf activity :observed-at))
                  now)))
       (values started-at started-at)))
    (t
     (values now now))))

(-> terminal-ui--status-times-at
    (terminal-ui real)
    (values integer integer))
(defun terminal-ui--status-times-at (ui now)
  "Return elapsed and idle whole seconds for UI's activity at monotonic NOW."
  (multiple-value-bind (started-at progress-at)
      (terminal-ui--status-timing-at ui now)
    (values (max 0 (floor (- now started-at)))
            (max 0 (floor (- now progress-at))))))


(-> terminal-ui--status-spinner-phase-at
    (terminal-ui real)
    (integer 0 3))
(defun terminal-ui--status-spinner-phase-at (ui now)
  "Return UI's quarter-second REPL status spinner phase at NOW."
  (let ((started-at (terminal-ui--status-timing-at ui now)))
    (mod (floor (* *terminal-ui-status-spinner-frames-per-second*
                   (max 0 (- now started-at))))
         4)))

(-> terminal-ui--status-spinner-spans-at (terminal-ui real) list)
(defun terminal-ui--status-spinner-spans-at (ui now)
  "Return UI's fixed-width READ/EVAL/PRINT/LOOP spinner spans at NOW."
  (multiple-value-bind (text bright-index)
      (ecase (terminal-ui--status-spinner-phase-at ui now)
        (0 (values "READ " 0))
        (1 (values "EVAL " 1))
        (2 (values "PRINT" 2))
        (3 (values "LOOP " 3)))
    (remove nil
            (list (and (plusp bright-index)
                       (terminal-span ':status-dim
                                      (subseq text 0 bright-index)))
                  (terminal-span ':status-plain
                                 (subseq text bright-index
                                         (1+ bright-index)))
                  (and (< (1+ bright-index) (length text))
                       (terminal-span ':status-dim
                                      (subseq text (1+ bright-index))))))))

(-> terminal-ui--compaction-track-width (integer) (integer 0))
(defun terminal-ui--compaction-track-width (row-width)
  "Return the compaction track width available within ROW-WIDTH cells."
  (min *terminal-ui-compaction-track-cells*
       (max 0
            (- row-width
               (text-cell-width "COMPACTING  [")
               (text-cell-width "]")))))

(-> terminal-ui--compaction-phase-at
    (terminal-ui real integer)
    (integer 0))
(defun terminal-ui--compaction-phase-at (ui now row-width)
  "Return UI's travelling compaction-head position at monotonic NOW."
  (let* ((track-width (terminal-ui--compaction-track-width row-width))
         (head-width  (min *terminal-ui-compaction-head-cells* track-width))
         (travel      (- track-width head-width))
         (started-at  (or (terminal-ui-compaction-started-at ui) now)))
    (if (plusp travel)
        (mod (floor (* *terminal-ui-status-spinner-frames-per-second*
                       (max 0 (- now started-at))))
             (1+ travel))
        0)))

(-> terminal-ui--running-job-p (terminal-ui) boolean)
(defun terminal-ui--running-job-p (ui)
  "Return true when UI presents at least one running command or child agent."
  (not
   (null
    (find ':running
          (append (terminal-ui--command-presentation-activities ui)
                  (terminal-ui-agent-activities ui))
          :key (lambda (activity)
                 (getf activity ':state))
          :test #'eq))))

(-> terminal-ui--status-signature-at (terminal-ui real) list)
(defun terminal-ui--status-signature-at (ui now)
  "Return the visible values identifying UI's status paint at NOW."
  (multiple-value-bind (elapsed idle)
      (terminal-ui--status-times-at ui now)
    (list elapsed
          (and (not (terminal-ui--running-job-p ui))
               (>= idle *terminal-ui-stale-status-seconds*)
               idle)
          (terminal-ui--status-spinner-phase-at ui now))))

(-> terminal-ui--animation-signature-at
    (terminal-ui real)
    (option list))
(defun terminal-ui--animation-signature-at (ui now)
  "Return visible status, compaction, command, child, and context values."
  (let ((commands (terminal-ui--command-presentation-activities ui))
        (agents (terminal-ui-agent-activities ui))
        (compacting-p (terminal-ui-compacting-p ui))
        (animated-p (terminal-ui--animated-status-row-p ui)))
    (when (terminal-ui--status-row-visible-p ui)
      (list
       (and animated-p
            (list (terminal-ui-status ui)
                  (terminal-ui-local-activity ui)
                  (terminal-ui--status-signature-at ui now)))
       (and compacting-p
            (terminal-ui--compaction-phase-at
             ui now (max 1 (terminal-columns (terminal-ui-terminal ui)))))
       (and commands
            (list
             (and (find ':running commands
                        :key (lambda (activity)
                               (getf activity :state))
                        :test #'eq)
                  (terminal-ui--agent-spinner-phase-at now))
             commands))
       (and agents
            (list
             (and (find ':running agents
                        :key (lambda (activity)
                               (getf activity :state))
                        :test #'eq)
                  (terminal-ui--agent-spinner-phase-at now))
             agents))
       (and (terminal-ui-context-window ui)
            (list (terminal-ui-context-used ui)
                  (terminal-ui-context-window ui)
                  (terminal-ui-context-compaction-limit ui)))))))

(-> terminal-ui--status-text-at (terminal-ui real) string)
(defun terminal-ui--status-text-at (ui now)
  "Return UI's activity timing at monotonic NOW."
  (multiple-value-bind (elapsed idle)
      (terminal-ui--status-times-at ui now)
    (if (and (not (terminal-ui--running-job-p ui))
             (>= idle *terminal-ui-stale-status-seconds*))
        (format nil "~A · no update ~A"
                (terminal-ui--duration-text elapsed)
                (terminal-ui--duration-text idle))
        (terminal-ui--duration-text elapsed))))

(-> terminal-ui--worked-spans-at (terminal-ui real) list)
(defun terminal-ui--worked-spans-at (ui now)
  "Return UI's total worked time spans at monotonic NOW, when tracked."
  (let ((worked-seconds (terminal-ui-status-worked-seconds ui)))
    (when worked-seconds
      (let ((elapsed (terminal-ui--status-times-at ui now)))
        (list (terminal-span ':status-dim "worked ")
              (terminal-span ':status-plain
                             (terminal-ui--duration-text
                              (+ worked-seconds elapsed))))))))

(-> terminal-ui--context-count-text ((integer 0)) string)
(defun terminal-ui--context-count-text (count)
  "Return token COUNT as a short whole-number context quantity."
  (cond
    ((< count 1000)
     (format nil "~D" count))
    ((< count 1000000)
     (format nil "~DK" (floor (+ count 500) 1000)))
    (t
     (let ((decimal (format nil "~,2F" (/ count 1000000))))
       (format nil "~AM" (string-right-trim "0." decimal))))))

(-> terminal-ui--context-compact-text (terminal-ui) string)
(defun terminal-ui--context-compact-text (ui)
  "Return UI's complete compact used/window context count."
  (format nil "~A/~A"
          (terminal-ui--context-count-text (terminal-ui-context-used ui))
          (terminal-ui--context-count-text (terminal-ui-context-window ui))))

(-> terminal-ui--context-track-spans (terminal-ui (integer 1)) list)
(defun terminal-ui--context-track-spans (ui track-width)
  "Return a used capacity track with UI's compaction marker."
  (let* ((used (min (terminal-ui-context-used ui)
                    (terminal-ui-context-window ui)))
         (window (terminal-ui-context-window ui))
         (limit (terminal-ui-context-compaction-limit ui))
         (used-cells (min track-width
                          (round (* used track-width) window)))
         (marker-at (min track-width
                         (round (* limit track-width) window))))
    (labels ((segment-spans (start end)
               "Return used and remaining spans from START through END."
               (let* ((width (- end start))
                      (used-width (max 0 (- (min used-cells end) start)))
                      (remaining-width (- width used-width)))
                 (append
                  (when (plusp used-width)
                    (list
                     (terminal-span
                      ':status-accent
                      (make-string used-width :initial-element #\=))))
                  (when (plusp remaining-width)
                    (list
                     (terminal-span
                      ':status-dim
                      (make-string remaining-width :initial-element #\.))))))))
      (append
       (list (terminal-span ':status-dim "["))
       (segment-spans 0 marker-at)
       (list (terminal-span ':status-plain "|"))
       (segment-spans marker-at track-width)
       (list (terminal-span ':status-dim "]"))))))

(-> terminal-ui--context-meter-spans (terminal-ui integer) list)
(defun terminal-ui--context-meter-spans (ui maximum-width)
  "Return UI's largest context meter variant fitting MAXIMUM-WIDTH cells."
  (let ((used (terminal-ui-context-used ui))
        (window (terminal-ui-context-window ui))
        (limit (terminal-ui-context-compaction-limit ui)))
    (when (and used window limit (plusp maximum-width))
      (let* ((used-text (terminal-ui--context-count-text used))
             (window-text (terminal-ui--context-count-text window))
             (full-prefix "ctx ")
             (full-suffix
               (format nil " ~A / ~A used" used-text window-text))
             (compact-prefix "")
             (compact-suffix
               (format nil " ~A" (terminal-ui--context-compact-text ui)))
             (full-fixed-width
               (+ (text-cell-width full-prefix)
                  3
                  (text-cell-width full-suffix)))
             (compact-fixed-width
               (+ 3 (text-cell-width compact-suffix))))
        (labels ((meter-spans (prefix suffix fixed-width)
                   "Return a context meter using its available track width."
                   (let ((track-width
                           (min *terminal-ui-context-track-cells*
                                (- maximum-width fixed-width))))
                     (append
                      (when (plusp (length prefix))
                        (list (terminal-span ':status-dim prefix)))
                      (terminal-ui--context-track-spans ui track-width)
                      (list (terminal-span ':status-dim suffix))))))
          (cond
            ((>= maximum-width (+ full-fixed-width 4))
             (meter-spans full-prefix full-suffix full-fixed-width))
            ((>= maximum-width (+ compact-fixed-width 4))
             (meter-spans compact-prefix compact-suffix compact-fixed-width))
            ((>= maximum-width
                 (text-cell-width (format nil "ctx~A" compact-suffix)))
             (list (terminal-span ':status-dim
                                  (format nil "ctx~A" compact-suffix))))
            ((>= maximum-width (1- (text-cell-width compact-suffix)))
             (list (terminal-span ':status-dim
                                  (subseq compact-suffix 1))))
            (t
             (terminal--clip-spans
              (list (terminal-span ':status-dim
                                   (format nil "ctx~A" compact-suffix)))
              maximum-width))))))))

(-> terminal-ui--compaction-head-text ((integer 1)) string)
(defun terminal-ui--compaction-head-text (width)
  "Return the right-pointing compaction head occupying WIDTH cells."
  (concatenate 'string
               (make-string (1- width) :initial-element #\=)
               ">"))

(-> terminal-ui--compaction-row-at (terminal-ui real integer) list)
(defun terminal-ui--compaction-row-at (ui now row-width)
  "Return UI's clipped indeterminate compaction row at monotonic NOW."
  (let* ((track-width (terminal-ui--compaction-track-width row-width))
         (head-width  (min *terminal-ui-compaction-head-cells* track-width))
         (head-at     (terminal-ui--compaction-phase-at ui now row-width))
         (content
           (if (plusp track-width)
               (remove
                nil
                (list
                 (terminal-span ':compaction-label "COMPACTING")
                 (terminal-span ':compaction-track "  [")
                 (and (plusp head-at)
                      (terminal-span
                       ':compaction-track
                       (make-string head-at :initial-element #\.)))
                 (terminal-span
                  ':compaction-head
                  (terminal-ui--compaction-head-text head-width))
                 (let ((remaining (- track-width head-at head-width)))
                   (and (plusp remaining)
                        (terminal-span
                         ':compaction-track
                         (make-string remaining :initial-element #\.))))
                 (terminal-span ':compaction-track "]")))
               (list (terminal-span ':compaction-label "COMPACTING"))))
         (clipped (terminal--clip-spans content row-width))
         (padding
           (and (terminal-styled-p (terminal-ui-terminal ui))
                (- row-width (terminal--spans-width clipped)))))
    (if (and padding (plusp padding))
        (append clipped
                (list
                 (terminal-span
                  ':compaction-track
                  (make-string padding :initial-element #\Space))))
        clipped)))

(-> terminal-ui--status-row-at (terminal-ui real integer) list)
(defun terminal-ui--status-row-at (ui now row-width)
  "Return UI's clipped status and context spans across ROW-WIDTH cells.

The context meter is right-aligned. Narrow rows reserve their available cells
for its complete used/window count before showing activity. Total worked time
joins the meter when every segment fits."
  (let* ((animated-p (terminal-ui--animated-status-row-p ui))
         (activity-content
           (and animated-p
                (append
                 (terminal-ui--status-spinner-spans-at ui now)
                 (list (terminal-span ':status-accent " ∙ ")
                       (terminal-span ':status-dim
                                      (terminal-ui--status-text-at ui now)))
                 (terminal-ui-status-details ui))))
         (content (or activity-content
                      (terminal-ui-idle-status-details ui)))
         (minimum-activity-width (text-cell-width "READ  ∙ 00:00"))
         (complete-context-width
           (and (terminal-ui-context-window ui)
                (text-cell-width (terminal-ui--context-compact-text ui))))
         (context-gap
           (if (and content
                    complete-context-width
                    (< row-width
                       (+ minimum-activity-width complete-context-width 2)))
               1
               2))
         (activity-context-budget
           (- row-width minimum-activity-width context-gap))
         (context
           (and (terminal-ui-context-window ui)
                (terminal-ui--context-meter-spans
                 ui
                 (if (and content
                          (>= activity-context-budget
                              (text-cell-width
                               (terminal-ui--context-compact-text ui))))
                     activity-context-budget
                     row-width))))
         (worked (and content (terminal-ui--worked-spans-at ui now))))
    (cond
      (context
       (let* ((context-width (terminal--spans-width context))
              (activity-space (- row-width context-width context-gap))
              (activity-p (and content
                               (>= activity-space minimum-activity-width)))
              (worked-with-context-p
                (and activity-p
                     worked
                     (<= (+ (terminal--spans-width content)
                            2
                            (terminal--spans-width worked)
                            2
                            context-width)
                         row-width)))
              (right
                (if worked-with-context-p
                    (append worked
                            (list (terminal-span ':status-plain "  "))
                            context)
                    context))
              (right-width (terminal--spans-width right))
              (left-limit
                (if activity-p
                    (max 0 (- row-width right-width context-gap))
                    0))
              (left (and activity-p
                         (terminal--clip-spans content left-limit)))
              (padding (- row-width
                          (terminal--spans-width left)
                          right-width)))
         (append
          left
          (when (plusp padding)
            (list (terminal-span
                   ':status-plain
                   (make-string padding :initial-element #\Space))))
          right)))
      (t
       (let* ((clipped (terminal--clip-spans content row-width))
              (left-width (terminal--spans-width clipped))
              (gap (- row-width left-width (terminal--spans-width worked)))
              (padding (and (terminal-styled-p (terminal-ui-terminal ui))
                            (- row-width left-width))))
         (cond
           ((and worked (>= gap 2))
            (append clipped
                    (list (terminal-span
                           ':status-plain
                           (make-string gap :initial-element #\Space)))
                    worked))
           ((and padding (plusp padding))
            (append clipped
                    (list (terminal-span
                           ':status-plain
                           (make-string padding :initial-element #\Space)))))
           (t
            clipped)))))))

(-> terminal-ui--local-activity-row (terminal-ui integer) list)
(defun terminal-ui--local-activity-row (ui row-width)
  "Return UI's explicit local Lisp activity row clipped to ROW-WIDTH."
  (terminal--clip-spans
   (list (terminal-span ':lisp-prompt "* ")
         (terminal-span ':plain (terminal-ui-local-activity ui)))
   row-width))

(-> terminal-ui--provider-activity-row (terminal-ui integer) list)
(defun terminal-ui--provider-activity-row (ui row-width)
  "Return UI's provider or tool activity row clipped to ROW-WIDTH."
  (terminal--clip-spans
   (list (terminal-span ':brand "∙ ")
         (terminal-span ':dim (terminal-ui-status ui)))
   row-width))

(-> terminal-ui--word-wrap-spans (terminal list integer) list)
(defun terminal-ui--word-wrap-spans (terminal spans row-width)
  "Return single-line SPANS as styled rows wrapped at word boundaries."
  (let* ((safe-spans
           (loop for span in spans
                 collect (terminal-span
                          (terminal-span-style span)
                          (sanitize-text (terminal-span-text span)
                                         :single-line-p t))))
         (text (terminal--spans-text safe-spans))
         (display (terminal--render-spans terminal safe-spans)))
    (loop for (row-text row-display) in (wrap-styled-text text
                                                          display
                                                          row-width)
          collect (terminal--make-rendered-row row-text row-display))))

(-> terminal-ui--stream-tail-rows (terminal (or string list) integer) list)
(defun terminal-ui--stream-tail-rows (terminal tail row-width)
  "Return TAIL as wrapped live rows for TERMINAL.

TAIL may be plain text, one styled row, or a list of styled rows. The multi-row
form keeps every speculative Markdown wrap live until the logical line commits."
  (let ((rows
          (cond
            ((stringp tail)
             (list (list (terminal-span ':plain tail))))
            ((terminal-styled-text-p tail)
             (list tail))
            ((and (listp tail)
                  (every #'terminal-styled-text-p tail))
             tail)
            (t
             (error 'terminal-error
                    :message "A stream tail must be text, styled spans, or styled rows."
                    :operation ':render
                    :cause nil)))))
    (loop for row in rows
          append (terminal-ui--word-wrap-spans terminal row row-width))))

(-> terminal-ui--row-content (terminal t) (values string string))
(defun terminal-ui--row-content (terminal row)
  "Return ROW's plain and styled content for TERMINAL."
  (if (terminal-rendered-row-p row)
      (values (terminal-rendered-row-text row)
              (terminal-rendered-row-display row))
      (values (terminal--spans-text row)
              (terminal--render-spans terminal row))))

(-> terminal-ui--rows-content
    (terminal list &key (:cursor-row integer) (:cursor-offset integer))
    (values string string integer))
(defun terminal-ui--rows-content
    (terminal rows &key (cursor-row 0) (cursor-offset 0))
  "Return ROWS as plain and styled text plus their cursor character index."
  (let ((plain-stream (make-string-output-stream))
        (display-stream (make-string-output-stream))
        (plain-length 0)
        (cursor-index nil))
    (loop for row in rows
          for index from 0
          do (multiple-value-bind (plain display)
                 (terminal-ui--row-content terminal row)
               (when (= index cursor-row)
                 (setf cursor-index
                       (+ plain-length
                          (min (max 0 cursor-offset) (length plain)))))
               (write-string plain plain-stream)
               (write-string display display-stream)
               (incf plain-length (length plain))
               (when (< (1+ index) (length rows))
                 (write-char #\Newline plain-stream)
                 (write-char #\Newline display-stream)
                 (incf plain-length))))
    (unless cursor-index
      (error 'terminal-error
             :message "The live-region cursor row is outside its content."
             :operation ':render
             :cause nil))
    (values (get-output-stream-string plain-stream)
            (get-output-stream-string display-stream)
            cursor-index)))

(-> terminal-ui--live-content
    (terminal-ui &optional (option real))
    (values string string integer))
(defun terminal-ui--live-content (ui &optional status-now)
  "Return UI's complete plain and styled live content plus its cursor index."
  (let* ((terminal (terminal-ui-terminal ui))
         (row-width (max 1 (terminal-columns terminal)))
         (status-row-p (terminal-ui--status-row-visible-p ui))
         (status-now (or status-now
                         (and (or status-row-p
                                  (terminal-ui-notice ui))
                              (funcall (terminal-ui-clock-function ui)))))
         (rows nil))
    (when status-row-p
      (setf rows
            (append rows
                    (list nil)
                    (when (terminal-ui-compacting-p ui)
                      (list
                       (terminal-ui--compaction-row-at
                        ui status-now row-width)))
                    (list
                     (terminal-ui--status-row-at
                      ui status-now row-width)))))
    (when (terminal-ui-local-activity ui)
      (setf rows
            (append rows
                    (list (terminal-ui--local-activity-row ui row-width)))))
    (let ((command-rows
            (and (terminal-ui--command-presentation-activities ui)
                 (terminal-ui--command-activity-rows-at
                  ui status-now row-width)))
          (agent-rows
            (and (terminal-ui-agent-activities ui)
                 (terminal-ui--agent-activity-rows-at
                  ui status-now row-width))))
      (setf rows (append rows command-rows agent-rows)))
    (when (terminal-ui-status ui)
      (setf rows
            (append rows
                    (list (terminal-ui--provider-activity-row ui row-width)))))
    (dolist (row (terminal-ui-preview-rows ui))
      (setf rows
            (append rows
                    (terminal-ui--word-wrap-spans terminal row row-width))))
    (when (terminal-ui-notice ui)
      (setf rows
            (append rows
                    (list
                     nil
                     (terminal--clip-spans
                      (list (terminal-span ':hint (terminal-ui-notice ui)))
                      row-width)))))
    (let ((steering-inputs (terminal-ui-steering-input-previews ui)))
      (when steering-inputs
        (setf rows
              (append
               rows
               (terminal-ui--pending-input-rows
                "steering"
                steering-inputs
                :count (length steering-inputs)
                :row-width row-width)))))
    (let ((queued-inputs (terminal-ui-queued-input-previews ui)))
      (when queued-inputs
        (setf rows
              (append
               rows
               (terminal-ui--pending-input-rows
                "follow-up"
                queued-inputs
                :count (length queued-inputs)
                :row-width row-width)
               (list
                (terminal--clip-spans
                 (list
                  (terminal-span ':hint
                                 "  Empty Tab edits newest; Shift-Tab cycles."))
                 row-width))))))
    (let ((tail (terminal-ui-stream-tail ui)))
      (when tail
        (setf rows
              (append rows
                      (terminal-ui--stream-tail-rows
                       terminal tail row-width)))))
    (when rows
      (setf rows (append rows (list nil))))
    (let ((selector (terminal-ui-selector ui)))
      (cond
        (selector
         (let* ((hint
                  (or (terminal-ui-selector-hint ui)
                      "enter selects, esc cancels"))
                (title-spans
                  (terminal--clip-spans
                   (list (terminal-span ':brand "∙ ")
                         (terminal-span ':plain
                                        (terminal-ui-selector-title ui))
                         (terminal-span ':hint (format nil "  ~A" hint)))
                   row-width)))
           (let ((cursor-row (length rows)))
             (setf rows
                   (append rows
                           (list title-spans
                                 nil)
                           (terminal-ui--choice-rows
                            selector
                            row-width)
                           (list nil)))
             (terminal-ui--rows-content
              terminal
              rows
              :cursor-row cursor-row
              :cursor-offset (length (terminal--spans-text title-spans))))))
        (t
         (multiple-value-bind (prompt-spans cursor-offset)
             (terminal-ui--prompt-content ui)
           (let ((cursor-row (length rows)))
             (setf rows
                   (append rows
                           (list prompt-spans)
                           (terminal-ui--completion-rows
                            ui row-width)
                           (list nil)))
             (terminal-ui--rows-content
              terminal
              rows
              :cursor-row cursor-row
              :cursor-offset cursor-offset))))))))

(-> terminal-ui--stream-output (terminal list) (values string string))
(defun terminal-ui--stream-output (terminal rows)
  "Return streamed ROWS as plain and styled output ending on a fresh line."
  (let ((plain-stream (make-string-output-stream))
        (display-stream (make-string-output-stream)))
    (dolist (row rows)
      (let ((safe-row
              (loop for span in row
                    collect (terminal-span
                             (terminal-span-style span)
                             (sanitize-text (terminal-span-text span)
                                            :single-line-p t)))))
        (write-string (terminal--spans-text safe-row) plain-stream)
        (write-string (terminal--render-spans terminal safe-row) display-stream)
        (write-char #\Newline plain-stream)
        (write-char #\Newline display-stream)))
    (values (get-output-stream-string plain-stream)
            (get-output-stream-string display-stream))))

(-> terminal-ui-stream-update
    (terminal-ui &key (:rows list) (:tail (or null string list)))
    terminal-ui)
(defun terminal-ui-stream-update (ui &key rows tail)
  "Append streamed single-line ROWS to the transcript and show TAIL as unfinished.

Each row is a styled span list appended once without a separating blank row, so
consecutive updates build one continuous transcript block. TAIL may be text, one
styled row, or styled rows; it replaces the live unfinished content, or NIL
removes it."
  (with-terminal-ui-locked (ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (multiple-value-bind (plain-output display-output)
          (terminal-ui--stream-output terminal rows)
        (setf (terminal-ui-stream-tail ui) tail)
        (if (terminal-interactive-p terminal)
            (terminal-ui--present-live
             ui
             :appended-text plain-output
             :appended-display display-output)
            (progn
              (when (plusp (length display-output))
                (terminal--write-safe-text terminal display-output))
              (terminal-ui--paint-live ui)))
        (terminal-flush terminal))))
  ui)

(-> terminal-ui--expire-notice-at (terminal-ui (option real)) boolean)
(defun terminal-ui--expire-notice-at (ui now)
  "Clear UI's notice when monotonic NOW reaches its deadline."
  (if (and now
           (terminal-ui-notice ui)
           (>= now (terminal-ui-notice-deadline ui)))
      (progn
        (setf (terminal-ui-notice ui) nil
              (terminal-ui-notice-deadline ui) nil)
        t)
      nil))

(-> terminal-ui--defer-live-append (terminal-ui string string) null)
(defun terminal-ui--defer-live-append (ui text display)
  "Retain appended plain TEXT and styled DISPLAY until live output resumes."
  (when (plusp (length text))
    (setf (terminal-ui-deferred-live-appended-text ui)
          (concatenate 'string
                       (terminal-ui-deferred-live-appended-text ui)
                       text)))
  (when (plusp (length display))
    (setf (terminal-ui-deferred-live-appended-display ui)
          (concatenate 'string
                       (terminal-ui-deferred-live-appended-display ui)
                       display)))
  nil)

(-> terminal-ui--note-command-paint (terminal-ui list) null)
(defun terminal-ui--note-command-paint (ui activities)
  "Mark ACTIVITIES visible and release painted pending command completions."
  (let ((identifiers
          (mapcar (lambda (activity)
                    (getf activity :id))
                  activities)))
    (setf (terminal-ui-command-unpainted-identifiers ui)
          (remove-if
           (lambda (identifier)
             (member identifier identifiers :test #'string=))
           (terminal-ui-command-unpainted-identifiers ui))
          (terminal-ui-command-pending-completions ui)
          (remove-if
           (lambda (activity)
             (member (getf activity :id) identifiers :test #'string=))
           (terminal-ui-command-pending-completions ui))))
  nil)

(-> terminal-ui--present-live
    (terminal-ui &key (:status-now (option real))
                      (:appended-text string)
                      (:appended-display string))
    null)
(defun terminal-ui--present-live
    (ui &key status-now (appended-text "") (appended-display ""))
  "Present UI live content, atomically preceding it with appended scrollback."
  (if (terminal-ui-live-output-suspended-p ui)
      (terminal-ui--defer-live-append ui appended-text appended-display)
      (let* ((appended-text
               (concatenate 'string
                            (terminal-ui-deferred-live-appended-text ui)
                            appended-text))
             (appended-display
               (concatenate 'string
                            (terminal-ui-deferred-live-appended-display ui)
                            appended-display))
             (status-now
               (or status-now
                   (and (or (terminal-ui--status-row-visible-p ui)
                            (terminal-ui-notice ui))
                        (funcall (terminal-ui-clock-function ui)))))
             (terminal (terminal-ui-terminal ui)))
        (terminal-ui--expire-notice-at ui status-now)
        (setf (terminal-ui-status-rendered-signature ui)
              (and status-now
                   (terminal-ui--animation-signature-at ui status-now)))
        (when (terminal-interactive-p terminal)
          (let ((painted-command-activities
                  (terminal-ui--command-visible-activities ui)))
            (multiple-value-bind (text display cursor)
                (terminal-ui--live-content ui status-now)
              (if (plusp (length appended-text))
                  (live-region-append-and-present
                   (terminal-ui-live-region ui)
                   appended-text
                   text
                   :appended-display appended-display
                   :cursor cursor
                   :display display)
                  (live-region-present (terminal-ui-live-region ui)
                                       text
                                       :cursor cursor
                                       :display display)))
            (terminal-ui--note-command-paint
             ui painted-command-activities)
            (setf (terminal-ui-deferred-live-appended-text ui) ""
                  (terminal-ui-deferred-live-appended-display ui) "")))))
  nil)

(-> terminal-ui--paint-live
    (terminal-ui &optional (option real))
    null)
(defun terminal-ui--paint-live (ui &optional status-now)
  "Present UI's unfinished content below ordinary terminal scrollback."
  (terminal-ui--present-live ui :status-now status-now)
  nil)

(-> terminal-ui--repaint-live (terminal-ui) null)
(defun terminal-ui--repaint-live (ui)
  "Recompose and repaint only UI's bounded live region."
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui--finalized-content
    (terminal-ui (or string list))
    (values string string))
(defun terminal-ui--finalized-content (ui entry)
  "Return finalized ENTRY as plain and styled text with a blank separator."
  (let* ((terminal (terminal-ui-terminal ui))
         (spans (if (stringp entry)
                    (list (terminal-span ':plain entry))
                    entry))
         (plain (terminal--spans-text spans))
         (display (terminal--render-spans terminal spans)))
    (unless (and (plusp (length plain))
                 (char= (char plain (1- (length plain))) #\Newline))
      (setf plain (concatenate 'string plain (string #\Newline))
            display (concatenate 'string display (string #\Newline))))
    (values (concatenate 'string plain (string #\Newline))
            (concatenate 'string display (string #\Newline)))))


(-> terminal-ui-refresh-size
    (terminal-ui (option function))
    boolean)
(defun terminal-ui-refresh-size (ui callback)
  "Apply CALLBACK's pending terminal size to UI and report whether it repainted."
  (let ((size (and callback (funcall callback))))
    (cond
      ((null size)
       nil)
      ((typep size '(cons (integer 1) (integer 1)))
       (terminal-ui-resize ui (rest size) :rows (first size))
       t)
      (t
       (error 'terminal-error
              :message "A terminal resize callback returned an invalid size."
              :operation ':resize
              :cause nil)))))

(-> terminal-ui-select
    (terminal-ui &key (:title string) (:items list)
                 (:hint (option string))
                 (:visible-count (integer 1))
                 (:initial-name (option string))
                 (:initial-value (option string))
                 (:search-p boolean)
                 (:search-key function)
                 (:resize-callback (option function))
                 (:on-event (option function))
                 (:poll-interval (option real)))
    (option string))
(defun terminal-ui-select
    (ui &key (title "select") items hint
             (visible-count *terminal-ui-visible-completions*)
             initial-name initial-value search-p
             (search-key #'terminal-ui--picker-default-search-text)
             resize-callback on-event
             (poll-interval *terminal-ui-picker-poll-interval*))
  "Run a modal picker over ITEMS and return the selected value, or NIL on cancel.

Items follow the completion entry shape and may carry a :VALUE distinct from
:NAME; entries without one return their display name. Up and Down move the
selection. Tab and Shift-Tab cycle it forward and backward, and Enter accepts
it. When SEARCH-P is true, inserted or pasted text filters candidates,
Backspace edits the query, and Ctrl-U clears it. Otherwise ordinary input
dismisses the picker with the selected item. Escape, Ctrl-C, or end of input
cancels. Returns NIL immediately when ITEMS is empty or the terminal is not
interactive.

VISIBLE-COUNT bounds candidate rows. INITIAL-NAME selects a matching display name,
and INITIAL-VALUE selects an explicit value before the first paint. HINT overrides
the default picker suffix. SEARCH-KEY returns the text searched for each item.

ON-EVENT, when provided, receives (EVENT SELECTOR) before search and default
handling and may return:
  NIL - fall through to search or ordinary selector handling
  :CONTINUE - custom handling already applied; continue the modal loop
  (:ACCEPT NAME) - accept NAME and close the picker
  (:CANCEL) - cancel and close the picker
  (:REPLACE TITLE ITEMS) - install a new title and item list, then continue
  (:REPLACE TITLE ITEMS HINT) - same, and replace the hint, including with NIL
  (:REPLACE TITLE ITEMS HINT VALUE) - same, selecting VALUE when it exists

RESIZE-CALLBACK is queried before each terminal-readiness check and immediately
before handling its resulting event. It returns positive pending rows and columns
as a cons, or NIL when no resize needs to be applied. POLL-INTERVAL defaults to
*TERMINAL-UI-PICKER-POLL-INTERVAL*; when non-NIL, it replaces the blocking read
with bounded readiness checks and delivers :POLL to ON-EVENT while no terminal
input is ready. Explicit NIL preserves blocking reads."
  (block nil
    (let ((source-items items)
          (base-title title)
          (search-query ""))
      (labels ((selected-value (item)
                 "Return ITEM's explicit value or its display name."
                 (or (getf item :value) (getf item :name)))

               (selected-designator ()
                 "Return the currently selected item's stable designator, or NIL."
                 (let* ((selector (terminal-ui-selector ui))
                        (selection (and selector
                                        (selector-selection selector)))
                        (selected (and selector
                                       (nth selection
                                            (selector-items selector)))))
                   (and selected (selected-value selected))))

               (select-designator (selector designator)
                 "Move SELECTOR to DESIGNATOR when that candidate exists."
                 (let ((position
                         (and designator
                              (position designator
                                        (selector-items selector)
                                        :key #'selected-value
                                        :test #'string=))))
                   (when position
                     (selector-move selector position))))

               (select-name (selector name)
                 "Move SELECTOR to the first displayed NAME when it exists."
                 (let ((position
                         (and name
                              (position name
                                        (selector-items selector)
                                        :key (lambda (entry)
                                               (getf entry :name))
                                        :test #'string=))))
                   (when position
                     (selector-move selector position))))

               (install-selector (next-items selected-designator)
                 "Install NEXT-ITEMS, retaining SELECTED-DESIGNATOR when possible."
                 (let ((selector
                         (make-selector
                          :items next-items
                          :visible-count visible-count
                          :arrangement ':vertical)))
                   (select-designator selector selected-designator)
                   (setf (terminal-ui-selector ui) selector
                         (terminal-ui-selector-title ui)
                          (terminal-ui--picker-search-title
                           base-title search-query (length next-items)))))

               (install
                   (next-title next-items
                    &optional (next-hint nil next-hint-supplied-p)
                              (next-value nil next-value-supplied-p))
                 "Install NEXT-TITLE, NEXT-ITEMS, optional hint, and selection on UI."
                 (unless (every #'terminal-completion-p next-items)
                   (return nil))
                 (setf source-items next-items
                       base-title next-title
                       search-query "")
                 (install-selector source-items
                                   (and next-value-supplied-p next-value))
                 (when next-hint-supplied-p
                   (setf (terminal-ui-selector-hint ui) next-hint))
                 t)

               (refresh-search ()
                 "Filter SOURCE-ITEMS through SEARCH-QUERY and preserve selection."
                 (let* ((selected-designator (selected-designator))
                        (matches
                          (if (plusp (length search-query))
                              (remove-if-not
                               (lambda (entry)
                                 (terminal-ui--picker-search-match-p
                                  entry search-query search-key))
                               source-items)
                              source-items)))
                   (install-selector matches selected-designator)))

               (append-search (text)
                 "Append display-safe TEXT to SEARCH-QUERY within its bound."
                 (let* ((safe-text (sanitize-text text :single-line-p t))
                        (remaining
                          (max 0
                               (- *terminal-ui-picker-search-character-limit*
                                  (length search-query))))
                        (addition
                          (subseq safe-text 0 (min remaining
                                                   (length safe-text)))))
                   (setf search-query
                         (concatenate 'string search-query addition))
                   (refresh-search)))

               (delete-search ()
                 "Delete the newest search grapheme and refresh candidates."
                 (when (plusp (length search-query))
                   (setf search-query
                         (subseq search-query
                                 0
                                 (grapheme-previous-boundary
                                  search-query (length search-query)))))
                 (refresh-search))

               (handle-search-event (event)
                 "Apply EVENT to picker search and return true when consumed."
                 (when search-p
                   (cond
                     ((and (consp event)
                           (member (first event) '(:insert :paste) :test #'eq)
                           (stringp (second event)))
                      (append-search (second event))
                      t)
                     ((eq event ':backspace)
                      (delete-search)
                      t)
                     ((eq event ':kill-line)
                      (setf search-query "")
                      (refresh-search)
                      t)
                     (t
                      nil)))))
        (unless (and items
                     (every #'terminal-completion-p items)
                     (terminal-interactive-p (terminal-ui-terminal ui)))
          (return nil))
        (with-terminal-ui-locked (ui)
          (setf (terminal-ui-selector-hint ui)
                (or hint
                    (and search-p
                         "type searches, backspace edits, enter selects, esc cancels")))
           (install title items)
           (if initial-value
               (select-designator (terminal-ui-selector ui) initial-value)
               (select-name (terminal-ui-selector ui) initial-name)))
        (unwind-protect
             (loop
               (with-terminal-ui-locked (ui)
                 (unless (terminal-ui-refresh-size ui resize-callback)
                   (terminal-ui--repaint-live ui)))
                (let ((event
                        (if (and poll-interval
                                 (not (terminal-input-ready-p
                                       (terminal-ui-terminal ui))))
                            (progn
                              (sleep poll-interval)
                              ':poll)
                            (terminal-read-event
                             (terminal-ui-terminal ui)))))
                 (when (eq event ':stream-end)
                   (return nil))
                 (with-terminal-ui-locked (ui)
                   (terminal-ui-refresh-size ui resize-callback)
                   (let ((custom
                           (and on-event
                                (funcall on-event event
                                         (terminal-ui-selector ui)))))
                     (cond
                       ((null custom)
                        (unless (or (eq event ':poll)
                                    (handle-search-event event))
                          (multiple-value-bind (action item)
                              (selector-handle-event
                               (terminal-ui-selector ui) event)
                            (case action
                              (:accept
                               (return (selected-value item)))
                              (:cancel
                               (return nil))
                              (:dismiss
                               (return (selected-value item)))
                              (t
                               nil)))))
                       ((eq custom ':continue)
                        nil)
                       ((and (consp custom) (eq (first custom) ':accept))
                        (return (second custom)))
                       ((and (consp custom) (eq (first custom) ':cancel))
                        (return nil))
                       ((and (consp custom) (eq (first custom) ':replace))
                        (apply #'install (rest custom)))
                       (t
                        (error 'terminal-error
                               :message
                               "ON-EVENT returned an unsupported picker action."
                               :operation ':select
                               :cause nil)))))))
          (with-terminal-ui-locked (ui)
            (setf (terminal-ui-selector ui) nil
                  (terminal-ui-selector-title ui) nil
                  (terminal-ui-selector-hint ui) nil)
             (terminal-ui--repaint-live ui)))))))


;;; Semantic prompt blocks

(-> terminal-ui-open-prompt-block (terminal-ui) boolean)
(defun terminal-ui-open-prompt-block (ui)
  "Emit prompt-start and input-start boundaries around one semantic prompt paint."
  (with-terminal-ui-locked (ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (when (and (terminal-ui-started-p ui)
                 (terminal-interactive-p terminal)
                 (eq (terminal-ui-prompt-marker-state ui) ':closed))
        (live-region-suspend (terminal-ui-live-region ui))
        (terminal-write-prompt-marker terminal ':prompt-start)
        (setf (terminal-ui-prompt-marker-state ui) ':prompt)
        (terminal-ui--paint-live ui)
        (terminal-write-prompt-marker terminal ':input-start)
        (setf (terminal-ui-prompt-marker-state ui) ':input)
        t))))

(-> terminal-ui-start-prompt-execution (terminal-ui) boolean)
(defun terminal-ui-start-prompt-execution (ui)
  "Emit one execution-start boundary for UI's current input block."
  (with-terminal-ui-locked (ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (when (and (terminal-ui-started-p ui)
                 (terminal-interactive-p terminal)
                 (eq (terminal-ui-prompt-marker-state ui) ':input))
        (terminal-write-prompt-marker terminal ':execution-start)
        (setf (terminal-ui-prompt-marker-state ui) ':executing)
        t))))

(-> terminal-ui-finish-prompt-block
    (terminal-ui &optional (integer 0))
    boolean)
(defun terminal-ui-finish-prompt-block (ui &optional (status 0))
  "Emit one completion boundary with STATUS for UI's executing prompt block."
  (with-terminal-ui-locked (ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (when (and (terminal-ui-started-p ui)
                 (terminal-interactive-p terminal)
                 (eq (terminal-ui-prompt-marker-state ui) ':executing))
        (terminal-write-prompt-marker terminal ':command-finished status)
        (setf (terminal-ui-prompt-marker-state ui) ':closed)
        t))))

;;;; -- Public UI Lifecycle and Events --

(-> terminal-ui-start (terminal-ui) terminal-ui)
(defun terminal-ui-start (ui)
  "Start UI on the primary screen and render its bounded live region."
  (with-terminal-ui-locked (ui)
    (unless (terminal-ui-started-p ui)
      (terminal-start (terminal-ui-terminal ui))
      (setf (terminal-ui-started-p ui) t)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-detach (terminal-ui) terminal-ui)
(defun terminal-ui-detach (ui)
  "Retract UI's live region after its terminal loses interactive ownership."
  (with-terminal-ui-locked (ui)
    (when (terminal-ui-started-p ui)
      (live-region-suspend (terminal-ui-live-region ui))))
  ui)

(-> terminal-ui-stop (terminal-ui) terminal-ui)
(defun terminal-ui-stop (ui)
  "Erase UI's unfinished rows and restore its terminal even after partial startup."
  (with-terminal-ui-locked (ui)
    (unwind-protect
         (when (terminal-ui-started-p ui)
           (when (eq (terminal-ui-prompt-marker-state ui) ':executing)
             (terminal-write-prompt-marker
              (terminal-ui-terminal ui) ':command-finished 1))
           (live-region-dismiss (terminal-ui-live-region ui)))
       (setf (terminal-ui-started-p ui) nil
             (terminal-ui-prompt-marker-state ui) ':closed
             (terminal-ui-notice ui) nil
             (terminal-ui-notice-deadline ui) nil
             (terminal-ui-live-output-suspended-p ui) nil)
       (terminal-stop (terminal-ui-terminal ui))))
  ui)

(defmacro with-terminal-ui ((variable ui-form) &body body)
  "Bind VARIABLE to UI-FORM, run BODY, and always restore its terminal state."
  `(let ((,variable ,ui-form))
     (unwind-protect
          (progn
            (terminal-ui-start ,variable)
            (locally
              ,@body))
       (terminal-ui-stop ,variable))))

(-> terminal-ui-mark-finalized (terminal-ui t) boolean)
(defun terminal-ui-mark-finalized (ui identifier)
  "Remember finalized IDENTIFIER and return true only on its first occurrence."
  (with-terminal-ui-locked (ui)
    (block nil
      (when (gethash identifier (terminal-ui-finalized-identifiers ui))
        (return nil))
      (setf (gethash identifier (terminal-ui-finalized-identifiers ui)) t)
      t)))

(-> terminal-ui-append-finalized (terminal-ui t (or string list)) boolean)
(defun terminal-ui-append-finalized (ui identifier entry)
  "Append finalized transcript ENTRY once for IDENTIFIER and return true when emitted."
  (with-terminal-ui-locked (ui)
    (block nil
      (when (gethash identifier (terminal-ui-finalized-identifiers ui))
        (return nil))
      (handler-case
          (multiple-value-bind (text display)
              (terminal-ui--finalized-content ui entry)
            (if (terminal-interactive-p (terminal-ui-terminal ui))
                (if (terminal-ui-live-output-suspended-p ui)
                    (terminal-ui--defer-live-append ui text display)
                    (live-region-append (terminal-ui-live-region ui)
                                        text
                                        :display display))
                (progn
                  (terminal--write-safe-text (terminal-ui-terminal ui) display)
                  (terminal-flush (terminal-ui-terminal ui))))
            (setf (gethash identifier
                           (terminal-ui-finalized-identifiers ui))
                  t))
        (error (condition)
          (remhash identifier (terminal-ui-finalized-identifiers ui))
          (error condition)))
      t)))

(-> terminal-ui-append-finalized-batch (terminal-ui list) (integer 0))
(defun terminal-ui-append-finalized-batch (ui entries)
  "Append ordered (IDENTIFIER ENTRY) pairs with one terminal-region update."
  (with-terminal-ui-locked (ui)
    (let ((pending nil)
          (seen (make-hash-table :test #'equal)))
      (dolist (pair entries)
        (destructuring-bind (identifier entry) pair
          (unless (or (gethash identifier
                               (terminal-ui-finalized-identifiers ui))
                      (gethash identifier seen))
            (multiple-value-bind (text display)
                (terminal-ui--finalized-content ui entry)
              (push (list identifier text display) pending)
              (setf (gethash identifier seen) t)))))
      (setf pending (nreverse pending))
      (when pending
        (let ((text-stream (make-string-output-stream))
              (display-stream (make-string-output-stream)))
          (dolist (entry pending)
            (write-string (second entry) text-stream)
            (write-string (third entry) display-stream))
          (let ((text (get-output-stream-string text-stream))
                (display (get-output-stream-string display-stream)))
            (handler-case
                (progn
                  (if (terminal-interactive-p (terminal-ui-terminal ui))
                      (if (terminal-ui-live-output-suspended-p ui)
                          (terminal-ui--defer-live-append ui text display)
                          (live-region-append (terminal-ui-live-region ui)
                                              text
                                              :display display))
                      (progn
                        (terminal--write-safe-text
                         (terminal-ui-terminal ui)
                         display)
                        (terminal-flush (terminal-ui-terminal ui))))
                  (dolist (entry pending)
                    (setf (gethash
                           (first entry)
                           (terminal-ui-finalized-identifiers ui))
                          t)))
              (error (condition)
                (dolist (entry pending)
                  (remhash (first entry)
                           (terminal-ui-finalized-identifiers ui)))
                (error condition))))))
      (length pending))))

(-> terminal-ui-set-preview-rows (terminal-ui list) terminal-ui)
(defun terminal-ui-set-preview-rows (ui rows)
  "Replace UI's transient styled ROWS and repaint only the live region."
  (unless (every #'terminal-styled-text-p rows)
    (error 'terminal-error
           :message "Every terminal preview row must contain styled spans."
           :operation ':set-preview
           :cause nil))
  (with-terminal-ui-locked (ui)
    (unless (equal rows (terminal-ui-preview-rows ui))
      (setf (terminal-ui-preview-rows ui) rows)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-notice
    (terminal-ui (option string) &key (:duration-seconds (option real)))
    (values terminal-ui boolean))
(defun terminal-ui-set-notice (ui notice &key duration-seconds)
  "Show NOTICE transiently for DURATION-SECONDS when UI is immediately available.

A contended presentation lock drops the notice rather than delaying emergency
terminal input or displaying a lifetime longer than the force-exit window. The
second value reports whether UI applied this update, so a caller that must be
seen can offer the notice again."
  (let ((safe-notice (and notice
                          (sanitize-text notice :single-line-p t)))
        (applied-p nil))
    (when (and safe-notice
               (or (null duration-seconds)
                   (not (plusp duration-seconds))))
      (error 'terminal-error
             :message "A terminal notice requires a positive duration."
             :operation ':set-notice
             :cause nil))
    (setf applied-p
          (nth-value
           1
           (terminal-ui--call-with-lock-if-available
            ui
            (lambda ()
              (cond
                (safe-notice
                 (let ((now (funcall (terminal-ui-clock-function ui))))
                   (setf (terminal-ui-notice ui) safe-notice
                         (terminal-ui-notice-deadline ui) (+ now duration-seconds))
                   (terminal-ui--paint-live ui now)))
                ((terminal-ui-notice ui)
                 (setf (terminal-ui-notice ui) nil
                       (terminal-ui-notice-deadline ui) nil)
                 (terminal-ui--paint-live ui)))))))
    (values ui applied-p)))

(-> terminal-ui-set-context-usage
    (terminal-ui
     &key (:used (integer 0)) (:window (integer 1))
          (:compaction-limit (integer 1)))
    terminal-ui)
(defun terminal-ui-set-context-usage (ui &key used window compaction-limit)
  "Set UI's provider-reported context usage, window, and compaction limit."
  (unless (<= compaction-limit window)
    (error 'terminal-error
           :message "The context compaction limit cannot exceed its window."
           :operation ':set-context-usage
           :cause nil))
  (with-terminal-ui-locked (ui)
    (unless (equal (list used window compaction-limit)
                   (list (terminal-ui-context-used ui)
                         (terminal-ui-context-window ui)
                         (terminal-ui-context-compaction-limit ui)))
      (setf (terminal-ui-context-used ui) used
            (terminal-ui-context-window ui) window
            (terminal-ui-context-compaction-limit ui) compaction-limit)
      (when (terminal-ui-started-p ui)
        (terminal-ui--paint-live ui))))
  ui)

(-> terminal-ui-set-compacting (terminal-ui boolean) terminal-ui)
(defun terminal-ui-set-compacting (ui compacting-p)
  "Begin or clear UI's animated indeterminate compaction indicator."
  (with-terminal-ui-locked (ui)
    (unless (eq compacting-p (terminal-ui-compacting-p ui))
      (cond
        (compacting-p
         (let ((now (funcall (terminal-ui-clock-function ui))))
           (setf (terminal-ui-compacting-p ui) t
                 (terminal-ui-compaction-started-at ui) now)
           (terminal-ui--paint-live ui now)))
        (t
         (setf (terminal-ui-compacting-p ui) nil
               (terminal-ui-compaction-started-at ui) nil)
         (terminal-ui--paint-live ui)))))
  ui)

(-> terminal-ui-set-idle-status-details
    (terminal-ui terminal-styled-text)
    terminal-ui)
(defun terminal-ui-set-idle-status-details (ui details)
  "Set UI's static session details shown on the idle modeline."
  (unless (terminal-styled-text-p details)
    (error 'terminal-error
           :message "Terminal idle status details must contain styled spans."
           :operation ':set-idle-status-details
           :cause nil))
  (with-terminal-ui-locked (ui)
    (unless (equal details (terminal-ui-idle-status-details ui))
      (setf (terminal-ui-idle-status-details ui) details)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-status
    (terminal-ui (option string)
     &key (:details terminal-styled-text)
          (:worked-seconds (option (integer 0))))
    terminal-ui)
(defun terminal-ui-set-status (ui status &key details worked-seconds)
  "Begin or clear UI's timed STATUS activity and animated modeline.

WORKED-SECONDS carries the conversation's accumulated working time at the
start of the activity; the live paint adds the running elapsed seconds."
  (unless (terminal-styled-text-p details)
    (error 'terminal-error
           :message "Terminal status details must contain styled spans."
           :operation ':set-status
           :cause nil))
  (with-terminal-ui-locked (ui)
    (let ((safe-status (and status
                            (sanitize-text status :single-line-p t))))
      (cond
        (safe-status
         (let ((now (funcall (terminal-ui-clock-function ui))))
           (setf (terminal-ui-status ui) safe-status
                 (terminal-ui-status-details ui) details
                 (terminal-ui-status-started-at ui) now
                 (terminal-ui-status-progress-at ui) now
                 (terminal-ui-status-worked-seconds ui) worked-seconds)
           (terminal-ui--paint-live ui now)))
        ((terminal-ui-status ui)
         (setf (terminal-ui-status ui) nil
               (terminal-ui-status-details ui) nil
               (terminal-ui-status-started-at ui) nil
               (terminal-ui-status-progress-at ui) nil
               (terminal-ui-status-worked-seconds ui) nil
               (terminal-ui-status-rendered-signature ui) nil)
         (terminal-ui--paint-live ui)))))
  ui)

(-> terminal-ui-set-local-activity
    (terminal-ui (option string))
    terminal-ui)
(defun terminal-ui-set-local-activity (ui activity)
  "Show or clear explicit local Lisp ACTIVITY below UI's animated status row."
  (with-terminal-ui-locked (ui)
    (let ((safe-activity
            (and activity (sanitize-text activity :single-line-p t))))
      (cond
        ((and safe-activity
              (not (equal safe-activity (terminal-ui-local-activity ui))))
         (let ((now (funcall (terminal-ui-clock-function ui))))
           (setf (terminal-ui-local-activity ui) safe-activity
                 (terminal-ui-local-activity-started-at ui) now)
           (terminal-ui--paint-live ui now)))
        ((and (null safe-activity) (terminal-ui-local-activity ui))
         (setf (terminal-ui-local-activity ui) nil
               (terminal-ui-local-activity-started-at ui) nil)
         (terminal-ui--paint-live ui)))))
  ui)

(-> terminal-ui-note-status-progress (terminal-ui) terminal-ui)
(defun terminal-ui-note-status-progress (ui)
  "Record current progress without restarting UI's elapsed activity clock."
  (with-terminal-ui-locked (ui)
    (when (terminal-ui-status ui)
      (setf (terminal-ui-status-progress-at ui)
            (funcall (terminal-ui-clock-function ui)))))
  ui)

(-> terminal-ui--sanitize-agent-activity (list real) list)
(defun terminal-ui--sanitize-agent-activity (activity observed-at)
  "Return a display-safe detached copy of one child ACTIVITY at OBSERVED-AT."
  (let ((current-tool (getf activity :current-tool)))
    (list :id
          (sanitize-text (getf activity :id) :single-line-p t)
          :index (getf activity :index)
          :agent
          (sanitize-text (getf activity :agent) :single-line-p t)
          :state (getf activity :state)
          :current-tool
          (and current-tool
               (sanitize-text current-tool :single-line-p t))
          :current-tool-duration-ms
          (getf activity :current-tool-duration-ms)
          :recent-tools
          (mapcar (lambda (tool)
                    (sanitize-text tool :single-line-p t))
                  (getf activity :recent-tools))
          :request-count (getf activity :request-count)
          :duration-ms (getf activity :duration-ms)
          :observed-at observed-at
          :assignment
          (sanitize-text (getf activity :assignment) :single-line-p t)
          :detached (not (null (getf activity :detached))))))

(-> terminal-ui-set-agent-activities (terminal-ui list) terminal-ui)
(defun terminal-ui-set-agent-activities (ui activities)
  "Replace UI's queued and running child-agent presentation state.

The responsive input reader coalesces worker notifications with animation
frames, so this function never paints directly from a child thread."
  (unless (every #'terminal-agent-activity-p activities)
    (error 'terminal-error
           :message "Every child-agent activity must be a valid live snapshot."
           :operation ':set-agent-activities
           :cause nil))
  (let* ((observed-at (funcall (terminal-ui-clock-function ui)))
         (safe-activities
           (sort
            (mapcar (lambda (activity)
                      (terminal-ui--sanitize-agent-activity
                       activity observed-at))
                    activities)
            #'<
            :key (lambda (activity)
                   (getf activity :index)))))
    (with-terminal-ui-locked (ui)
      (unless (equal safe-activities (terminal-ui-agent-activities ui))
        (setf (terminal-ui-agent-activities ui) safe-activities))))
  ui)

(-> terminal-ui--sanitize-command-activity (list real) list)
(defun terminal-ui--sanitize-command-activity (activity observed-at)
  "Return a display-safe detached copy of one command ACTIVITY at OBSERVED-AT."
  (list :id
        (sanitize-text (getf activity :id) :single-line-p t)
        :type ':tool
        :index (getf activity :index)
        :tool
        (sanitize-text (getf activity :tool) :single-line-p t)
        :description
        (sanitize-text (getf activity :description) :single-line-p t)
        :state (getf activity :state)
        :duration-ms (getf activity :duration-ms)
        :observed-at observed-at
        :detached (not (null (getf activity :detached)))))

(-> terminal-ui--bounded-command-completions (list) list)
(defun terminal-ui--bounded-command-completions (activities)
  "Return ACTIVITIES in job order within the pending completion bound."
  (let ((ordered
          (sort (copy-list activities)
                #'<
                :key (lambda (activity)
                       (getf activity :index)))))
    (subseq ordered
            0
            (min (length ordered)
                 *terminal-ui-command-pending-completion-limit*))))

(-> terminal-ui-set-command-activities (terminal-ui list) terminal-ui)
(defun terminal-ui-set-command-activities (ui activities)
  "Replace UI's queued and running primary command presentation state.

Commands that finish before their first reader-owned paint are retained
individually within a fixed bound. This function never paints from a worker
thread."
  (unless (every #'terminal-command-activity-p activities)
    (error 'terminal-error
           :message "Every command activity must be a valid live snapshot."
           :operation ':set-command-activities
           :cause nil))
  (let* ((observed-at (funcall (terminal-ui-clock-function ui)))
         (safe-activities
           (sort
            (mapcar (lambda (activity)
                      (terminal-ui--sanitize-command-activity
                       activity observed-at))
                    activities)
            #'<
            :key (lambda (activity)
                   (getf activity :index)))))
    (with-terminal-ui-locked (ui)
      (let ((current (terminal-ui-command-activities ui)))
        (unless (equal safe-activities current)
          (let* ((current-unpainted
                   (terminal-ui-command-unpainted-identifiers ui))
                 (new-identifiers
                   (mapcar (lambda (activity)
                             (getf activity :id))
                           safe-activities))
                 (removed-unpainted
                   (loop for activity in current
                         for identifier = (getf activity :id)
                         when (and
                               (member identifier current-unpainted
                                       :test #'string=)
                               (not (member identifier new-identifiers
                                            :test #'string=)))
                           collect activity))
                 (removed-identifiers
                   (mapcar (lambda (activity)
                             (getf activity :id))
                           removed-unpainted))
                 (pending
                   (remove-if
                    (lambda (activity)
                      (let ((identifier (getf activity :id)))
                        (or (member identifier new-identifiers :test #'string=)
                            (member identifier removed-identifiers
                                    :test #'string=))))
                    (terminal-ui-command-pending-completions ui)))
                 (next-unpainted
                   (loop for activity in safe-activities
                         for identifier = (getf activity :id)
                         when (or
                               (member identifier current-unpainted
                                       :test #'string=)
                               (not (find identifier current
                                          :key (lambda (old-activity)
                                                 (getf old-activity :id))
                                          :test #'string=)))
                           collect identifier)))
            (setf (terminal-ui-command-activities ui) safe-activities
                  (terminal-ui-command-unpainted-identifiers ui)
                  next-unpainted
                  (terminal-ui-command-pending-completions ui)
                  (terminal-ui--bounded-command-completions
                   (append pending removed-unpainted))))))))
  ui)

(-> terminal-ui-clear-command-activities (terminal-ui) terminal-ui)
(defun terminal-ui-clear-command-activities (ui)
  "Unconditionally discard every current and pending primary command row."
  (with-terminal-ui-locked (ui)
    (setf (terminal-ui-command-activities ui) nil
          (terminal-ui-command-unpainted-identifiers ui) nil
          (terminal-ui-command-pending-completions ui) nil))
  ui)

(-> terminal-ui-refresh-status (terminal-ui) boolean)
(defun terminal-ui-refresh-status (ui)
  "Repaint changed live timing without waiting for a contended presentation lock."
  (multiple-value-bind (repainted-p acquired-p)
      (terminal-ui--call-with-lock-if-available
       ui
       (lambda ()
          (let* ((status-now
                   (and (or (terminal-ui--status-row-visible-p ui)
                            (terminal-ui-notice ui))
                        (funcall (terminal-ui-clock-function ui))))
                (notice-expired-p
                  (terminal-ui--expire-notice-at ui status-now))
                (signature nil))
           (setf signature
                 (and status-now
                      (terminal-ui--animation-signature-at ui status-now)))
           (if (and (not notice-expired-p)
                    (equal signature
                           (terminal-ui-status-rendered-signature ui)))
               nil
               (progn
                 (terminal-ui--paint-live ui status-now)
                 t)))))
    (and acquired-p (not (null repainted-p)))))

(-> terminal-ui-set-pending-inputs (terminal-ui list list) terminal-ui)
(defun terminal-ui-set-pending-inputs (ui steering-inputs queued-inputs)
  "Set UI's pending input previews and repaint them at most once."
  (let ((safe-steering (mapcar #'sanitize-text steering-inputs))
        (safe-queued (mapcar #'sanitize-text queued-inputs)))
    (with-terminal-ui-locked (ui)
      (unless (and (equal safe-steering
                          (terminal-ui-steering-input-previews ui))
                   (equal safe-queued
                          (terminal-ui-queued-input-previews ui)))
        (setf (terminal-ui-steering-input-previews ui) safe-steering
              (terminal-ui-queued-input-previews ui) safe-queued)
        (terminal-ui--paint-live ui))))
  ui)

(-> terminal-ui-recall-follow-up
    (terminal-ui (or string user-message-input)
     &key (:steering-inputs list) (:queued-inputs list))
    terminal-ui)
(defun terminal-ui-recall-follow-up
    (ui input &key steering-inputs queued-inputs)
  "Recall INPUT into UI while atomically refreshing pending input previews."
  (let ((safe-steering (mapcar #'sanitize-text steering-inputs))
        (safe-queued (mapcar #'sanitize-text queued-inputs)))
    (with-terminal-ui-locked (ui)
      (setf (terminal-ui-steering-input-previews ui) safe-steering
            (terminal-ui-queued-input-previews ui) safe-queued)
      (terminal-ui--set-draft-input ui input)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-input
    (terminal-ui (or string user-message-input)) terminal-ui)
(defun terminal-ui-set-input (ui input)
  "Replace UI's editable input with INPUT and repaint it."
  (with-terminal-ui-locked (ui)
    (terminal-ui--set-draft-input ui input)
    (setf (terminal-ui-completion-dismissed-p ui) nil)
    (terminal-ui--paint-live ui))
  ui)

(-> terminal-ui-set-lisp-input (terminal-ui boolean) terminal-ui)
(defun terminal-ui-set-lisp-input (ui lisp-input-p)
  "Set whether UI is explicitly reading Common Lisp input and repaint it."
  (with-terminal-ui-locked (ui)
    (setf (terminal-ui-lisp-input-p ui) lisp-input-p)
    (terminal-ui--paint-live ui))
  ui)

(-> terminal-ui-set-cursor-visible (terminal-ui boolean) terminal-ui)
(defun terminal-ui-set-cursor-visible (ui visible-p)
  "Set whether UI leaves its input cursor visible between terminal updates."
  (with-terminal-ui-locked (ui)
    (when (terminal-interactive-p (terminal-ui-terminal ui))
      (live-region-set-cursor-visible (terminal-ui-live-region ui) visible-p)))
  ui)

(-> terminal-ui-resize
    (terminal-ui integer &key (:rows (option integer)))
    terminal-ui)
(defun terminal-ui-resize (ui columns &key rows)
  "Set UI terminal dimensions and repaint only unfinished rows."
  (with-terminal-ui-locked (ui)
    (let* ((new-columns (max 1 columns))
           (terminal    (terminal-ui-terminal ui))
           (region      (terminal-ui-live-region ui)))
      (terminal-set-dimensions terminal new-columns :rows rows)
      (live-region-resize
       region
       new-columns
       :maximum-rows (terminal-ui--maximum-live-rows terminal)
       :repaint-p nil)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-read-event (terminal-ui) t)
(defun terminal-ui-read-event (ui)
  "Read one semantic input event for UI without emitting fallback prompt controls."
  (terminal-read-event (terminal-ui-terminal ui)))

(-> terminal-ui--safe-editor-event (t) t)
(defun terminal-ui--safe-editor-event (event)
  "Return EVENT with direct text input sanitized for terminal presentation."
  (if (and (consp event)
           (member (first event) '(:insert :paste :line))
           (consp (rest event))
           (stringp (second event)))
      (list (first event) (sanitize-text (second event)))
      event))

(-> terminal-ui--apply-editor-event
    (terminal-ui t)
    (values keyword (option (or string user-message-input))))
(defun terminal-ui--apply-editor-event (ui event)
  "Apply EVENT through Clinedi while preserving Autolith interaction policy."
  (let ((editor (terminal-ui-editor ui)))
    (cond
      ((and (eq event :interrupt)
            (plusp (length (line-editor-text editor))))
       (line-editor-clear editor)
       (setf (terminal-ui-image-attachments ui) nil)
       (values :cleared nil))
      ((and (consp event)
            (eq (first event) :paste)
            (stringp (second event))
            (terminal-ui--attach-pasted-image ui (second event)))
       (values :changed nil))
      ((eq event :complete)
       (line-editor-handle-event editor '(:insert "    "))
       (values :changed nil))
      ((eq event :edit-queue)
       (values :edit-queue nil))
      ((eq event :keep-queue-edit)
       (values :kept nil))
      ((eq event :cycle-queue)
       (values :cycle-queue
               (terminal-ui--submission-input
                ui (line-editor-text editor))))
      ((member event '(:up :down))
       (let* ((terminal (terminal-ui-terminal ui))
              (prompt-width
                (text-cell-width
                 (sanitize-text (terminal-ui-prompt ui)
                                :single-line-p t)))
              (direction (if (eq event :up) -1 1)))
         (if (line-editor-move-vertical
              editor direction
              :columns (terminal-columns terminal)
              :prompt-width prompt-width)
             (values :changed nil)
             (multiple-value-bind (action payload)
                 (line-editor-handle-event
                  editor
                  (if (eq event :up)
                      ':history-previous
                      ':history-next))
               (values (if (eq action :continue) ':changed action)
                       payload)))))
      ((eq event :queue-submit)
       (multiple-value-bind (action payload)
           (line-editor-handle-event editor :submit)
         (values (if (eq action :submit) ':queue action) payload)))
      ((eq event :clear-screen)
       (values :changed nil))
      (t
       (multiple-value-bind (action payload)
           (line-editor-handle-event
            editor
            (terminal-ui--safe-editor-event event))
         (values (if (eq action :continue) ':changed action)
                 payload))))))

(-> terminal-ui-process-event
    (terminal-ui t &key (:queue-completion-p boolean) (:queue-editing-p boolean))
    (values keyword (option (or string user-message-input))))
(defun terminal-ui-process-event
    (ui event &key queue-completion-p queue-editing-p)
  "Apply EVENT to UI's suggestions or editor and return its action and payload."
  (with-terminal-ui-locked (ui)
    (let* ((editor (terminal-ui-editor ui))
           ;; The editor installs a fresh string on every text change, so
           ;; holding the reference preserves the pre-event content.
           (text-before (line-editor-text editor))
           (images-before
             (terminal-ui--copy-image-attachments
              (terminal-ui-image-attachments ui)))
           (completion-items
             (and (member event '(:complete :complete-previous))
                  (terminal-ui--reconcile-completions ui)))
           (effective-event
             (cond
               ((and (eq event :complete-previous)
                     queue-editing-p
                     (not (terminal-ui-completion-active-p ui)))
                ':cycle-queue)
               ((and (eq event :complete)
                     (null completion-items))
                (cond
                  ((and queue-editing-p
                        (zerop (length (line-editor-text editor))))
                   ':keep-queue-edit)
                  ((not queue-completion-p)
                   ':submit)
                  ((plusp (length (line-editor-text editor)))
                   ':queue-submit)
                  (t
                   ':edit-queue)))
               (t
                event))))
      (multiple-value-bind (completion-action completion-payload)
          (terminal-ui--handle-completion-event ui effective-event)
        (if completion-action
            (values completion-action completion-payload)
            (multiple-value-bind (action payload)
                (terminal-ui--apply-editor-event ui effective-event)
              (when (eq action :changed)
                (terminal-ui--restore-history-images ui))
              (let ((text-after (line-editor-text editor)))
                (when (and (eq action :changed)
                           (not (eq text-before text-after))
                           (not (string= text-before text-after)))
                  (setf (terminal-ui-completion-dismissed-p ui) nil)))
              (when (and (member action '(:submit :queue))
                         (stringp payload))
                (terminal-ui--remember-image-submission
                 ui text-before images-before)
                (setf payload (terminal-ui--submission-input
                               ui payload)
                      (terminal-ui-image-attachments ui) nil))
              (when (member action '(:changed :cleared :submit :queue))
                (terminal-ui--repaint-live ui))
              (values action payload)))))))
