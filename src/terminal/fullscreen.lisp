(in-package #:autolith)

;;;; -- Fullscreen Viewport --

(defparameter *fullscreen-transcript-character-limit* 4194304
  "The most characters of transcript text, display and rows the fullscreen viewport keeps.

Older output is evicted past it and stays loadable with (history).")

(defclass fullscreen-terminal-ui (terminal-ui)
  ((viewport :initform      nil
             :accessor      fullscreen-terminal-ui-viewport
             :documentation "The reflowing transcript, its scroll position and its last layout.")
   (evicted-sequences :initform      (make-hash-table :test #'equal)
                      :reader        fullscreen-terminal-ui-evicted-sequences
                      :documentation "The newest evicted durable sequence by conversation identifier.")
   (active-p :initform nil :accessor fullscreen-terminal-ui-active-p :type boolean
             :documentation "Whether this UI has entered the alternate buffer.")
   (platform-token :initform nil :accessor fullscreen-terminal-ui-platform-token
                   :documentation "Native output mode to restore after leaving the buffer.")
   (painter :initform (clinedi:make-frame-painter) :reader fullscreen-terminal-ui-painter
            :documentation "Paints frames by rewriting the rows that changed since the last one.")
   (cursor-visible-p :initform t :accessor fullscreen-terminal-ui-cursor-visible-p
                     :type boolean :documentation "Requested composer cursor visibility.")
   (welcome-tip :initform nil :accessor fullscreen-terminal-ui-welcome-tip
                :documentation "Startup advice retained across welcome repaints and resize.")
   (welcome-p :initform nil :accessor terminal-ui-fullscreen-welcome-p :type boolean
              :documentation "Whether the empty session shows the machine console panel.")
   (epilogue :initform nil :accessor fullscreen-terminal-ui-epilogue :type (option string)
             :documentation "Rendered text written to the normal screen once the alternate buffer is left."))
  (:documentation "An alternate-screen transcript viewport and bottom-pinned composer."))

(defmethod shared-initialize :after ((ui fullscreen-terminal-ui) slot-names &key)
  "Give UI its bounded transcript viewport, also when another UI changes class into it."
  (declare (ignore slot-names))
  (unless (fullscreen-terminal-ui-viewport ui)
    (setf (fullscreen-terminal-ui-viewport ui) (terminal-ui-fullscreen--make-viewport ui 80))))

(defmethod terminal-ui-fullscreen-p ((ui fullscreen-terminal-ui))
  "Use application-owned transcript scrolling for this UI."
  t)

(-> terminal-ui-fullscreen--make-viewport (fullscreen-terminal-ui (integer 1)) clinedi:transcript-viewport)
(defun terminal-ui-fullscreen--make-viewport (ui width)
  "Return an empty viewport WIDTH columns wide that forgets the records it evicts."
  (clinedi:make-transcript-viewport
   :width           width
   :character-limit *fullscreen-transcript-character-limit*
   :evict-function  (lambda (identifiers)
                      (terminal-ui-fullscreen--forget-evicted ui identifiers))))

(-> terminal-ui-fullscreen--forget-evicted (fullscreen-terminal-ui list) null)
(defun terminal-ui-fullscreen--forget-evicted (ui identifiers)
  "Forget the finalized IDENTIFIERS of evicted output and note their newest sequences.

History paging can then present those records again."
  (dolist (identifier identifiers)
    (terminal-ui--forget-finalized ui identifier)
    (when (and (consp identifier)
               (eq (first identifier) ':conversation)
               (integerp (third identifier)))
      (let ((owner (second identifier)))
        (setf (gethash owner (fullscreen-terminal-ui-evicted-sequences ui))
              (max (third identifier)
                   (gethash owner (fullscreen-terminal-ui-evicted-sequences ui) 0))))))
  nil)

(defmethod terminal-ui--note-finalized-output ((ui fullscreen-terminal-ui) identifiers)
  "Tag the newest transcript chunk with IDENTIFIERS so its eviction forgets them."
  (let ((viewport (fullscreen-terminal-ui-viewport ui)))
    (setf (clinedi:transcript-viewport-last-tag viewport)
          (append identifiers (clinedi:transcript-viewport-last-tag viewport))))
  nil)

(defmethod terminal-ui-take-evicted-sequence ((ui fullscreen-terminal-ui) owner)
  "Return and clear the newest evicted sequence of conversation OWNER."
  (with-terminal-ui-locked (ui)
    (let ((sequence (gethash owner (fullscreen-terminal-ui-evicted-sequences ui))))
      (remhash owner (fullscreen-terminal-ui-evicted-sequences ui))
      sequence)))

(-> terminal-ui-fullscreen-invalidate (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-invalidate (ui)
  "Invalidate UI's physical frame without discarding its transcript or scroll anchor."
  (clinedi:frame-painter-invalidate (fullscreen-terminal-ui-painter ui))
  nil)

(defmethod terminal-ui-reset-transcript ((ui fullscreen-terminal-ui))
  "Replace the fullscreen transcript and repaint a fresh, tail-following view."
  (with-terminal-ui-locked (ui)
    (setf (fullscreen-terminal-ui-viewport ui)
          (terminal-ui-fullscreen--make-viewport
           ui (max 1 (terminal-columns (terminal-ui-terminal ui))))
          (terminal-ui-deferred-live-appended-text ui) ""
          (terminal-ui-deferred-live-appended-display ui) ""
          (terminal-ui-deferred-live-appended-regions ui) nil
          (terminal-ui-stream-tail ui) nil
          (terminal-ui-compact-tail-group ui) nil
          (terminal-ui-pending-action ui) nil
          (terminal-ui-notice ui) nil
          (terminal-ui-notice-deadline ui) nil
          (terminal-ui-fullscreen-welcome-p ui) nil)
    (terminal-ui--clear-finalized ui)
    (clrhash (fullscreen-terminal-ui-evicted-sequences ui))
    (terminal-ui-fullscreen-invalidate ui)
    (when (terminal-ui-started-p ui)
      (terminal-ui--paint-live ui)))
  nil)

(-> terminal-ui-fullscreen-leave (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-leave (ui)
  "Leave only an owned alternate buffer and restore native state on every exit.

Any epilogue set through TERMINAL-UI-SET-EPILOGUE follows the restore
sequence, so it lands in the normal screen where it stays readable."
  (unwind-protect
       (terminal-ui--leave-theme ui)
    (when (fullscreen-terminal-ui-active-p ui)
      (unwind-protect
           (let ((terminal (terminal-ui-terminal ui))
                 (epilogue (shiftf (fullscreen-terminal-ui-epilogue ui) nil)))
             (terminal--write
              terminal
              (concatenate
               'string
               (format nil "~C[0m~C[?7h~C[?25h" #\Escape #\Escape #\Escape)
               (mouse-reporting-disable-sequence)
               (terminal-keyboard-enhancement-disable-sequence)
               (alternate-screen-leave-sequence)))
             (when epilogue
               (terminal--write-safe-text terminal (format nil "~A~%" epilogue)))
             (terminal-flush terminal))
        (let ((token (fullscreen-terminal-ui-platform-token ui)))
          (setf (fullscreen-terminal-ui-active-p ui) nil
                (fullscreen-terminal-ui-platform-token ui) nil)
          (terminal-ui-fullscreen-invalidate ui)
          (platform-terminal-restore-fullscreen *platform* token)))))
  nil)

(-> terminal-ui-fullscreen-enter (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-enter (ui)
  "Enter the alternate buffer once, unwinding partial startup before propagating failure."
  (when (and (terminal-interactive-p (terminal-ui-terminal ui))
             (not (fullscreen-terminal-ui-active-p ui)))
    (let ((token       (platform-terminal-enable-fullscreen *platform*))
          (completed-p nil))
      (unwind-protect
           (progn
             (setf (fullscreen-terminal-ui-platform-token ui) token
                   (fullscreen-terminal-ui-active-p ui) t)
             (terminal--write
              (terminal-ui-terminal ui)
              (concatenate
               'string
               (alternate-screen-enter-sequence)
               (terminal-keyboard-enhancement-enable-sequence)
               (format nil "~C[2J~C[H" #\Escape #\Escape)
               (mouse-reporting-enable-sequence)))
             (terminal-ui--enter-theme ui)
             (terminal-flush (terminal-ui-terminal ui))
             (terminal-ui-fullscreen-invalidate ui)
             (setf completed-p t))
        (unless completed-p
          ; mallet:suppress no-ignore-errors -- best-effort restoration must not mask the entry failure
          (ignore-errors (terminal-ui-fullscreen-leave ui))))))
  nil)


;;;; -- Incremental Transcript and Reflow --

(-> terminal-ui-fullscreen--ensure-width (fullscreen-terminal-ui integer) null)
(defun terminal-ui-fullscreen--ensure-width (ui width)
  "Reflow the transcript only after a width change, keeping its visible anchor."
  (when (clinedi:transcript-viewport-resize (fullscreen-terminal-ui-viewport ui) width)
    (terminal-ui-fullscreen-invalidate ui))
  nil)

(-> terminal-ui-fullscreen--append
    (fullscreen-terminal-ui string string &key (:regions list))
    null)
(defun terminal-ui-fullscreen--append (ui text display &key regions)
  "Extend the transcript with one committed output chunk and its click REGIONS."
  (when (plusp (length text))
    (clinedi:transcript-viewport-append (fullscreen-terminal-ui-viewport ui)
                                        text display :regions regions)
    (setf (terminal-ui-fullscreen-welcome-p ui) nil))
  nil)

(-> terminal-ui-fullscreen--display-rows (terminal-ui list integer) list)
(defun terminal-ui-fullscreen--display-rows (ui rows width)
  "Wrap styled live ROWS into independent trusted display strings."
  (loop for row in rows
        append (multiple-value-bind (plain display)
                   (terminal-ui--row-content (terminal-ui-terminal ui) row)
                 (mapcar #'second (clinedi:wrap-styled-text plain display width)))))

(-> terminal-ui-fullscreen--composer (fullscreen-terminal-ui integer integer)
    (values list integer integer))
(defun terminal-ui-fullscreen--composer (ui width height)
  "Return a cursor-containing composer window, bounded to part of the screen."
  (multiple-value-bind (rows cursor-row cursor-offset) (terminal-ui--composer-rows ui)
    (multiple-value-bind (text display cursor)
        (terminal-ui--rows-content (terminal-ui-terminal ui) rows
                                  :cursor-row cursor-row :cursor-offset cursor-offset)
      ;; Rows and cursor come from one row model, so a space at a flush wrap
      ;; keeps its cell instead of shifting the cursor past the text.
      (multiple-value-bind (rows row column)
          (clinedi:wrap-styled-editor-rows text display :cursor cursor :columns width)
        (let* ((lines (mapcar #'second rows))
               (limit (max 1 (if (terminal-ui-selector ui)
                                 (- height 2)
                                 (floor height 2))))
               (count (min limit (length lines)))
               (start (min (max 0 (- row (1- count)))
                           (max 0 (- (length lines) count)))))
          (values (subseq lines start (+ start count)) (- row start) column))))))

(-> terminal-ui-fullscreen--legend-items (boolean &key (:evicted-p boolean)) list)
(defun terminal-ui-fullscreen--legend-items (following-p &key evicted-p)
  "Return (KEY DESCRIPTION) pairs for the legend, for a tail-following or scrolled view.

EVICTED-P adds how to load output older than the viewport keeps."
  (append
   '(("PgUp/PgDn" "scroll")
     ("Ctrl-PgUp/PgDn" "messages"))
   (if following-p
       '(("Ctrl-Home/End" "ends")
         ("Shift+mouse" "to select text"))
       '(("Ctrl-End" "to follow")))
   (when evicted-p
     '(("(history)" "earlier output")))))

(-> terminal-ui-fullscreen--legend-row
    (fullscreen-terminal-ui &key (:top integer) (:total integer) (:space integer)
                            (:width integer))
    string)
(defun terminal-ui-fullscreen--legend-row (ui &key top total space width)
  "Render the legend separating the transcript from the composer.

The position block wears the legend accent, keys the plain legend style, and
their descriptions the dim legend style. These share the modeline's
foregrounds without its background, so the legend stays distinct from the
composer's dim placeholder without reading as a second modeline."
  (let* ((viewport    (fullscreen-terminal-ui-viewport ui))
         (following-p (clinedi:transcript-viewport-following-p viewport))
         (evicted-p   (and (not following-p)
                           (zerop top)
                           (plusp (clinedi:transcript-viewport-evicted-count viewport))))
         (position    (if following-p
                          " LIVE "
                          (format nil " ~D-~D / ~D "
                                  (1+ top) (min total (+ top space)) total)))
         (spans
           (append
            (list (terminal-span ':legend-accent position))
            (loop for (key description) in (terminal-ui-fullscreen--legend-items
                                             following-p :evicted-p evicted-p)
                  for first-p = t then nil
                  append (list (terminal-span ':legend-dim (if first-p "  " " · "))
                               (terminal-span ':legend-plain key)
                               (terminal-span ':legend-dim
                                              (format nil " ~A" description)))))))
    (terminal--render-spans (terminal-ui-terminal ui)
                            (terminal--clip-spans spans width))))

(-> terminal-ui-fullscreen--prompt-prefix
    (fullscreen-terminal-ui &key (:width integer) (:limit (integer 0)) (:now (option real)))
    list)
(defun terminal-ui-fullscreen--prompt-prefix (ui &key width limit now)
  "Bound prompt-adjacent rows to LIMIT, omitting their leading transcript gap."
  (let* ((rows    (terminal-ui-fullscreen--display-rows
                   ui (terminal-ui--prompt-prefix-rows ui now) width))
         (visible (member-if (lambda (row) (plusp (length row))) rows))
         (count   (min limit (if visible (1+ (length visible)) 0))))
    (subseq (append visible (when visible (list ""))) 0 count)))

(-> terminal-ui-fullscreen--frame (fullscreen-terminal-ui (option real))
    (values list integer integer))
(defun terminal-ui-fullscreen--frame (ui status-now)
  "Compose scrollable transcript output above pinned activity and input rows."
  (let* ((terminal (terminal-ui-terminal ui))
         (width    (max 1 (terminal-columns terminal)))
         (height   (max 1 (terminal-rows terminal))))
    (terminal-ui-fullscreen--ensure-width ui width)
    (multiple-value-bind (composer cursor-row cursor-column)
        (terminal-ui-fullscreen--composer ui width height)
      (let* ((viewport    (fullscreen-terminal-ui-viewport ui))
             (separator-p (> height (length composer)))
             (available   (max 0 (- height (length composer) (if separator-p 1 0))))
             ;; Keep a transcript row even when activity fills the available space.
             (prefix      (terminal-ui-fullscreen--prompt-prefix
                           ui :width width :limit (max 0 (1- available)) :now status-now))
             (space       (- available (length prefix)))
             (committed   (clinedi:transcript-viewport-row-count viewport))
             (tail        (terminal-ui-stream-tail ui))
             (live        (coerce
                           (terminal-ui-fullscreen--display-rows
                            ui (when tail
                                 (append (when (terminal-ui-compact-tail-group ui) (list nil))
                                         (terminal-ui--stream-tail-rows terminal tail width)))
                            width)
                           'vector))
             (total       (+ committed (length live)))
             (top         (clinedi:transcript-viewport-layout viewport space
                                                              :extra-rows (length live)))
             (visible
               (if (and (zerop total) (terminal-ui-fullscreen-welcome-p ui))
                   (terminal-ui--welcome-rows ui space)
                   (loop for index from top below (min total (+ top space))
                         collect (if (< index committed)
                                     (clinedi:transcript-viewport-row-display viewport index)
                                     (aref live (- index committed)))))))
        (values
         (append visible (make-list (max 0 (- space (length visible))) :initial-element "")
                 (when separator-p
                   (list (terminal-ui-fullscreen--legend-row
                          ui :top top :total total :space space :width width)))
                 prefix composer)
         (+ space (if separator-p 1 0) (length prefix) cursor-row) cursor-column)))))


;;;; -- Physical Frames and Navigation --

(-> terminal-ui-fullscreen-paint
    (fullscreen-terminal-ui &key (:rows list) (:cursor-row integer) (:cursor-column integer)) null)
(defun terminal-ui-fullscreen-paint (ui &key rows (cursor-row 0) (cursor-column 0))
  "Diff trusted display-string ROWS at absolute positions, without scrolling the terminal.

ROWS are already wrapped to the terminal width by their composers, so they are
compared and written as they are; the frame disables autowrap, which clips any
row that still overruns instead of scrolling."
  (when (fullscreen-terminal-ui-active-p ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (clinedi:frame-painter-paint
       (fullscreen-terminal-ui-painter ui)
       rows
       (lambda (controls)
         (terminal--write terminal controls)
         (terminal-flush terminal))
       :height           (max 1 (terminal-rows terminal))
       :width            (max 1 (terminal-columns terminal))
       :cursor-row       cursor-row
       :cursor-column    cursor-column
       :cursor-visible-p (and (fullscreen-terminal-ui-cursor-visible-p ui)
                              (not (terminal-ui-boot-waiting-p ui))))))
  nil)

(-> terminal-ui-fullscreen-scroll (fullscreen-terminal-ui integer) null)
(defun terminal-ui-fullscreen-scroll (ui delta)
  "Move DELTA rows through history; positive values move towards the latest output."
  (clinedi:transcript-viewport-scroll (fullscreen-terminal-ui-viewport ui) delta)
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui-fullscreen-bottom (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-bottom (ui)
  "Follow the latest transcript output again."
  (clinedi:transcript-viewport-follow (fullscreen-terminal-ui-viewport ui))
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui-fullscreen--message-start-p (fullscreen-terminal-ui string (integer 0)) boolean)
(defun terminal-ui-fullscreen--message-start-p (ui text offset)
  "Return true when TEXT's row at OFFSET starts a line opening a user or assistant message.

A line opens a message when it begins with a message prefix, or when it is an
indented timestamp line directly above one, as in the REPL transcript style."
  (flet ((prefixed-p (start)
           (some (lambda (prefix)
                   (let ((end (+ start (length prefix))))
                     (and (<= end (length text))
                          (string= prefix text :start2 start :end2 end))))
                 (terminal-ui-message-header-prefixes ui))))
    (and (or (zerop offset)
             (char= (char text (1- offset)) #\Newline))
         (or (prefixed-p offset)
             (let ((newline (position #\Newline text :start offset)))
               (and newline
                    (< (+ offset 2) newline)
                    (string= "  " text :start2 offset :end2 (+ offset 2))
                    (prefixed-p (1+ newline)))))
         t)))

(-> terminal-ui-fullscreen-jump-message (fullscreen-terminal-ui (member -1 1)) null)
(defun terminal-ui-fullscreen-jump-message (ui direction)
  "Scroll to the neighboring message in DIRECTION, ending it on the bottom row.

DIRECTION -1 moves to the message before the one on the bottom row and 1 to
the one after it. The message ends where the reader's eyes already are, next
to the composer; one taller than the window shows its header on top. A
message inside the final window follows the tail again. Without a message in
that direction the viewport does not move."
  (when (clinedi:transcript-viewport-jump
         (fullscreen-terminal-ui-viewport ui)
         direction
         (lambda (text offset)
           (terminal-ui-fullscreen--message-start-p ui text offset))
         :align ':end)
    (terminal-ui--paint-live ui))
  nil)

(-> terminal-ui-fullscreen--click-action
    (fullscreen-terminal-ui integer integer)
    list)
(defun terminal-ui-fullscreen--click-action (ui column row)
  "Return the transcript action under one-based screen COLUMN and ROW, or NIL.

A widget region covering the clicked committed character wins, otherwise a
web URL under it opens. Live rows, the separator, and the composer have no
actions."
  (multiple-value-bind (action text index)
      (clinedi:transcript-viewport-hit (fullscreen-terminal-ui-viewport ui) column row)
    (when index
      (or action
          (let ((url (url-at text index)))
            (and url (list ':open-url url)))))))

(-> terminal-ui-fullscreen--click (fullscreen-terminal-ui integer integer) boolean)
(defun terminal-ui-fullscreen--click (ui column row)
  "Queue the transcript action under COLUMN and ROW, reporting whether one exists.

The action runs after TERMINAL-UI-PROCESS-EVENT releases the presentation
lock, because copying and browser launches present notices of their own."
  (let ((action (terminal-ui-fullscreen--click-action ui column row)))
    (when action
      (setf (terminal-ui-pending-action ui) action))
    (not (null action))))

(-> terminal-ui-fullscreen-handle-event (fullscreen-terminal-ui t) boolean)
(defun terminal-ui-fullscreen-handle-event (ui event)
  "Consume viewport navigation without changing the draft or its history position."
  (block nil
    (cond
      ((eq event ':page-up)
       (terminal-ui-fullscreen-scroll
        ui (- (clinedi:transcript-viewport-page-rows (fullscreen-terminal-ui-viewport ui)))))
      ((eq event ':page-down)
       (terminal-ui-fullscreen-scroll
        ui (clinedi:transcript-viewport-page-rows (fullscreen-terminal-ui-viewport ui))))
      ((eq event ':scroll-top)
       (clinedi:transcript-viewport-scroll-to-top (fullscreen-terminal-ui-viewport ui))
       (terminal-ui--paint-live ui))
      ((eq event ':scroll-bottom)
       (terminal-ui-fullscreen-bottom ui))
      ((eq event ':previous-section)
       (terminal-ui-fullscreen-jump-message ui -1))
      ((eq event ':next-section)
       (terminal-ui-fullscreen-jump-message ui 1))
      ((and (consp event) (eq (first event) ':scroll) (member (second event) '(-1 1)))
       (terminal-ui-fullscreen-scroll ui (* 3 (second event))))
      ((typep event '(cons (eql :click) (cons (integer 1) (cons (integer 1) null))))
       (terminal-ui-fullscreen--click ui (second event) (third event)))
      ((eq event ':clear-screen)
       (terminal-ui-fullscreen-invalidate ui)
       (terminal-ui--paint-live ui))
      (t
       (when (and (consp event) (member (first event) '(:insert :paste :line)))
         (setf (terminal-ui-fullscreen-welcome-p ui) nil))
       (return nil)))
    t))


;;;; -- Presentation Transactions --

(defmethod terminal-ui-set-epilogue ((ui fullscreen-terminal-ui) entry)
  "Retain ENTRY's rendering for the normal screen; the alternate buffer would discard it."
  (with-terminal-ui-locked (ui)
    (setf (fullscreen-terminal-ui-epilogue ui)
          (terminal--render-spans (terminal-ui-terminal ui)
                                  (if (stringp entry)
                                      (list (terminal-span ':plain entry))
                                      entry))))
  nil)

(defmethod terminal-ui--append-output
    ((ui fullscreen-terminal-ui) text display
     &key regions compact-first-group compact-last-group)
  "Commit output through the same transaction as streamed live presentation."
  (declare (ignore compact-first-group compact-last-group))
  (terminal-ui--present-live ui :appended-text text :appended-display display
                                :appended-regions regions))

(defmethod terminal-ui--present-live
    ((ui fullscreen-terminal-ui) &key status-now (appended-text "") (appended-display "")
                                 appended-regions)
  "Append, paint and commit atomically; a failed paint can be retried without duplication."
  (if (terminal-ui-live-output-suspended-p ui)
      (terminal-ui--defer-live-append ui appended-text appended-display
                                      :regions appended-regions)
      (let* ((terminal      (terminal-ui-terminal ui))
             (deferred-text (terminal-ui-deferred-live-appended-text ui))
             (text          (concatenate 'string deferred-text appended-text))
             (display       (concatenate 'string (terminal-ui-deferred-live-appended-display ui) appended-display))
             (regions       (append (terminal-ui-deferred-live-appended-regions ui)
                                    (termdown:shift-regions appended-regions (length deferred-text)))))
        (terminal-ui-fullscreen--ensure-width ui (max 1 (terminal-columns terminal)))
        (let ((checkpoint  (clinedi:transcript-viewport-checkpoint
                            (fullscreen-terminal-ui-viewport ui)))
              (welcome-p   (terminal-ui-fullscreen-welcome-p ui))
              (completed-p nil))
          (unwind-protect
               (progn
                 (terminal-ui-fullscreen--append ui text display :regions regions)
                 (when (terminal-ui-started-p ui)
                   (if (terminal-interactive-p terminal)
                       (progn
                         (terminal-ui-fullscreen-enter ui)
                         (unless (terminal-ui-theme-defaults-current-p ui)
                           (terminal-ui--enter-theme ui))
                         (let ((now (or status-now (funcall (terminal-ui-clock-function ui)))))
                           (terminal-ui--expire-notice-at ui now)
                           (setf (terminal-ui-status-rendered-signature ui)
                                 (terminal-ui--animation-signature-at ui now))
                           (multiple-value-bind (rows cursor-row cursor-column)
                               (terminal-ui-fullscreen--frame ui now)
                             (terminal-ui-fullscreen-paint
                              ui :rows rows :cursor-row cursor-row :cursor-column cursor-column))
                           (terminal-ui--note-command-paint ui (terminal-ui--command-visible-activities ui))))
                       (when (and (plusp (length display))
                                  (terminal-plain-output-p terminal))
                         (terminal--write-safe-text terminal display)
                         (terminal-flush terminal))))
                 (setf (terminal-ui-deferred-live-appended-text ui) ""
                       (terminal-ui-deferred-live-appended-display ui) ""
                       (terminal-ui-deferred-live-appended-regions ui) nil
                       completed-p t))
            (unless completed-p
              (clinedi:transcript-viewport-rollback (fullscreen-terminal-ui-viewport ui)
                                                    checkpoint)
              (setf (terminal-ui-fullscreen-welcome-p ui) welcome-p)
              (terminal-ui-fullscreen-invalidate ui))))))
  nil)
