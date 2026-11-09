(in-package #:autolith)

;;;; -- Fullscreen Viewport --

(defparameter *fullscreen-transcript-character-limit* 4194304
  "The maximum characters of transcript text, display and rows kept in the viewport.

When output exceeds the limit, older output is evicted and can be loaded with (history).")

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
               :documentation "Rendered text written to the normal screen once the alternate buffer is left.")
   (presentation-records :initform      (make-hash-table :test #'equal)
                         :accessor      fullscreen-terminal-ui-presentation-records
                         :documentation "Resident call spans indexed by chunk, call and job identity.")
   (pinned-actions :initform      nil
                   :accessor      fullscreen-terminal-ui-pinned-actions
                   :documentation "Last frame's pinned widget hit regions in screen cells.")
   (markdown-stream :initform      nil
                    :accessor      fullscreen-terminal-ui-markdown-stream
                    :documentation "The identity of the unfinished semantic Markdown publication, or NIL.")
   (evicted-markdown-stream :initform      nil
                            :accessor      fullscreen-terminal-ui-evicted-markdown-stream
                            :documentation "One evicted active identity suppressed until its block finishes.")
   (deferred-operations :initform      nil
                        :accessor      fullscreen-terminal-ui-deferred-operations
                        :documentation "Reverse-ordered output and record batches awaiting publication.")
   (deferred-base-group :initform      nil
                        :accessor      fullscreen-terminal-ui-deferred-base-group
                        :documentation "Separator state before the first queued output operation.")
   (press :initform      nil
          :accessor      fullscreen-terminal-ui-press
          :documentation "The (COLUMN ROW) of the left button held down in the transcript, or NIL.")
   (dragged-p :initform      nil
              :accessor      fullscreen-terminal-ui-dragged-p
              :type          boolean
              :documentation "Whether the pointer moved while the button was held, selecting text."))
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

(-> terminal-ui-fullscreen--forget-sequence (fullscreen-terminal-ui t) null)
(defun terminal-ui-fullscreen--forget-sequence (ui identifier)
  "Forget IDENTIFIER and advance its conversation's evicted history floor."
  (terminal-ui--forget-finalized ui identifier)
  (when (and (consp identifier) (eq (first identifier) ':conversation)
             (integerp (third identifier)))
    (let ((owner (second identifier)))
      (setf (gethash owner (fullscreen-terminal-ui-evicted-sequences ui))
            (max (third identifier)
                 (gethash owner (fullscreen-terminal-ui-evicted-sequences ui) 0)))))
  nil)

(-> terminal-ui-fullscreen--forget-evicted (fullscreen-terminal-ui list) null)
(defun terminal-ui-fullscreen--forget-evicted (ui identifiers)
  "Forget evicted chunks, their associated outcomes and all resident identity aliases."
  (let ((index (fullscreen-terminal-ui-presentation-records ui)))
    (dolist (identifier identifiers)
      (let ((record (gethash identifier index)))
        (when record
          (when (equal identifier (fullscreen-terminal-ui-markdown-stream ui))
            (setf (fullscreen-terminal-ui-evicted-markdown-stream ui) identifier
                  (fullscreen-terminal-ui-markdown-stream ui) nil
                  (terminal-ui-stream-tail ui) nil))
          (dolist (key (list identifier (getf record :call-key) (getf record :job-key)))
            (when key (remhash key index)))
          (dolist (outcome (getf record :identifiers))
            (terminal-ui-fullscreen--forget-sequence ui outcome))))
      (terminal-ui-fullscreen--forget-sequence ui identifier)))
  nil)

(defmethod terminal-ui--note-finalized-output ((ui fullscreen-terminal-ui) identifiers)
  "Tag the corresponding output, deferring its identity with suspended output."
  (if (fullscreen-terminal-ui-deferred-operations ui)
      (push (list ':tag identifiers) (fullscreen-terminal-ui-deferred-operations ui))
      (terminal-ui-fullscreen--tag-last ui identifiers))
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
  (terminal-screen-invalidate (terminal-ui-terminal ui) (fullscreen-terminal-ui-painter ui))
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
          (fullscreen-terminal-ui-markdown-stream ui) nil
          (fullscreen-terminal-ui-evicted-markdown-stream ui) nil
          (terminal-ui-compact-tail-group ui) nil
          (terminal-ui-pending-action ui) nil
          (terminal-ui-notice ui) nil
          (terminal-ui-notice-deadline ui) nil
          (terminal-ui-fullscreen-welcome-p ui) nil)
    (clrhash (fullscreen-terminal-ui-presentation-records ui))
    (setf (fullscreen-terminal-ui-deferred-operations ui) nil
          (fullscreen-terminal-ui-deferred-base-group ui) nil)
    (terminal-ui--clear-finalized ui)
    (clrhash (fullscreen-terminal-ui-evicted-sequences ui))
    (terminal-ui-fullscreen-invalidate ui)
    (when (terminal-ui-started-p ui)
      (terminal-ui--paint-live ui)))
  nil)

(-> terminal-ui-fullscreen-leave (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-leave (ui)
  "Leave only an owned alternate buffer and restore native state on every exit.

An epilogue set through TERMINAL-UI-SET-EPILOGUE is written to the normal
screen after leaving the alternate buffer."
  (unwind-protect
       (terminal-ui--leave-theme ui)
    (when (fullscreen-terminal-ui-active-p ui)
      (let ((terminal (terminal-ui-terminal ui))
            (epilogue (shiftf (fullscreen-terminal-ui-epilogue ui) nil))
            (token    (shiftf (fullscreen-terminal-ui-platform-token ui) nil)))
        (unwind-protect
             (unwind-protect
                  (terminal-screen-leave terminal token)
               (when epilogue
                 (terminal--write-safe-text terminal (format nil "~A~%" epilogue))
                 (terminal-flush terminal)))
          (setf (fullscreen-terminal-ui-active-p ui) nil)
          (terminal-ui-fullscreen-invalidate ui)))))
  nil)

(-> terminal-ui-fullscreen-enter (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-enter (ui)
  "Enter the alternate buffer once, unwinding partial startup before propagating failure."
  (when (and (terminal-interactive-p (terminal-ui-terminal ui))
             (not (fullscreen-terminal-ui-active-p ui)))
    (let ((completed-p nil))
      (unwind-protect
           (progn
             (setf (fullscreen-terminal-ui-active-p ui) t
                   (fullscreen-terminal-ui-platform-token ui)
                   (terminal-screen-enter (terminal-ui-terminal ui)))
             (terminal-ui--enter-theme ui)
             (terminal-flush (terminal-ui-terminal ui))
             (terminal-ui-fullscreen-invalidate ui)
             (setf completed-p t))
        (unless completed-p
          ; mallet:suppress no-ignore-errors -- best-effort restoration must not mask the entry failure
          (ignore-errors (terminal-ui-fullscreen-leave ui))))))
  nil)


;;;; -- Incremental Transcript and Reflow --

(-> terminal-ui-fullscreen--markdown-record (fullscreen-terminal-ui list) (option list))
(defun terminal-ui-fullscreen--markdown-record (ui tag)
  "Return the resident semantic Markdown record named by any identifier in TAG."
  (loop for identifier in tag
        for record = (gethash identifier (fullscreen-terminal-ui-presentation-records ui))
        when (getf record :markdown) return record))

(-> terminal-ui-fullscreen--markdown-content
    (fullscreen-terminal-ui list integer) (values string string list list))
(defun terminal-ui-fullscreen--markdown-content (ui record width)
  "Render RECORD's original Markdown at WIDTH, retaining its structural separators."
  (multiple-value-bind (spans tail)
      (terminal-ui--markdown-render (getf record :markdown) width)
    (let ((prefix (getf record :prefix "")))
      (multiple-value-bind (text display regions)
          (if (getf (getf record :markdown) :stream-p)
              (values (terminal--spans-text spans)
                      (terminal--render-spans (terminal-ui-terminal ui) spans)
                      (termdown:widget-regions spans))
              (terminal-ui--finalized-content ui spans :group (getf record :group)))
        (values (concatenate 'string prefix text)
                (concatenate 'string prefix display)
                (termdown:shift-regions regions (length prefix))
                tail)))))

(-> terminal-ui-fullscreen--charge-markdown (fullscreen-terminal-ui &key (:all-p boolean)) null)
(defun terminal-ui-fullscreen--charge-markdown (ui &key all-p)
  "Charge retained Markdown to its resident chunk, then apply the existing eviction policy.

The pinned Clinedi viewport recomputes all chunk weights on replace and resize;
ALL-P adds source weight once after those operations. Append charges only the
newest chunk. Keep this adapter next to the bulk reflow adapter below."
  (let* ((viewport (fullscreen-terminal-ui-viewport ui))
         (chunks   (clinedi::transcript-viewport--chunks viewport)))
    (loop for index from (if all-p 0 (max 0 (1- (length chunks)))) below (length chunks)
          for chunk = (aref chunks index)
          for record = (terminal-ui-fullscreen--markdown-record ui (clinedi::viewport-chunk-tag chunk))
          for markdown = (getf record :markdown)
          when markdown
            do (let ((weight (+ (length (getf markdown :source))
                                (loop for span in (getf markdown :header)
                                      sum (length (terminal-span-text span))))))
                 (incf (clinedi::viewport-chunk-weight chunk) weight)
                 (incf (clinedi::transcript-viewport--weight viewport) weight)))
    (clinedi::viewport--evict viewport))
  nil)

(-> terminal-ui-fullscreen--ensure-width (fullscreen-terminal-ui integer) null)
(defun terminal-ui-fullscreen--ensure-width (ui width)
  "Replay resident semantic sources once at WIDTH, preserving chunk/offset anchors.

Clinedi 58b has no bulk replacement operation. Stage immutable chunks through
this narrow adapter, then use its single resize pass for rows, anchoring and
weights. Offsets identify rendered characters, not Markdown source positions."
  (let ((viewport (fullscreen-terminal-ui-viewport ui)))
    (unless (= width (clinedi:transcript-viewport-width viewport))
      (let* ((previous          (clinedi::transcript-viewport--chunks viewport))
             (chunks            (make-array (length previous) :adjustable t :fill-pointer t
                                                              :initial-contents previous))
             (stream-rendered-p nil))
        (loop for index below (length chunks)
              for chunk = (aref chunks index)
              for tag = (clinedi::viewport-chunk-tag chunk)
              for record = (terminal-ui-fullscreen--markdown-record ui tag)
              when record
                do (multiple-value-bind (text display regions tail)
                       (terminal-ui-fullscreen--markdown-content ui record width)
                     (setf (aref chunks index)
                           (clinedi::%make-viewport-chunk text display regions tag))
                     (when (equal (getf record :identifier)
                                  (fullscreen-terminal-ui-markdown-stream ui))
                      (setf (terminal-ui-stream-tail ui) tail
                            stream-rendered-p t))))
        (unless stream-rendered-p
          (let ((record (gethash (fullscreen-terminal-ui-markdown-stream ui)
                                (fullscreen-terminal-ui-presentation-records ui))))
            (when record
              (setf (terminal-ui-stream-tail ui)
                    (nth-value 3 (terminal-ui-fullscreen--markdown-content ui record width))))))
        ;; Render every source before changing the retained viewport representation.
        (setf (clinedi::transcript-viewport--chunks viewport) chunks)
        (clinedi:transcript-viewport-resize viewport width)
        (terminal-ui-fullscreen--charge-markdown ui :all-p t)
        (terminal-ui-fullscreen-invalidate ui))))
  nil)

(defmethod terminal-ui-stream-markdown
    ((ui fullscreen-terminal-ui) identifier &key source header marker finished-p)
  "Replace one streamed publication from source without changing its transcript order."
  (with-terminal-ui-locked (ui)
    (terminal-ui-fullscreen--submit
     ui (list ':markdown identifier
              (list :source source :header header :marker marker
                    :finished-p finished-p :stream-p t))))
  ui)

(-> terminal-ui-fullscreen--apply-markdown (fullscreen-terminal-ui t list) string)
(defun terminal-ui-fullscreen--apply-markdown (ui identifier markdown)
  "Publish MARKDOWN's committed rows and live tail under stable IDENTIFIER."
  (block nil
    (when (equal identifier (fullscreen-terminal-ui-evicted-markdown-stream ui))
      (when (getf markdown :finished-p)
        (setf (fullscreen-terminal-ui-evicted-markdown-stream ui) nil))
      (return ""))
    (let* ((previous (gethash identifier (fullscreen-terminal-ui-presentation-records ui)))
           (record   (if previous (copy-list previous)
                         (list :identifier identifier :tag (list identifier)
                               :identifiers (list identifier) :group nil
                               :prefix (if (terminal-ui--separated-p
                                            (terminal-ui-compact-tail-group ui) nil)
                                           (string #\Newline) "")))))
      (setf (getf record :markdown) markdown)
      (multiple-value-bind (text display regions tail)
          (terminal-ui-fullscreen--markdown-content
           ui record (max 1 (terminal-columns (terminal-ui-terminal ui))))
        (setf (getf record :published-p) (or (getf previous :published-p) (plusp (length text))))
        (terminal-ui-fullscreen--index-record ui record)
        (setf (terminal-ui-stream-tail ui) tail
              (fullscreen-terminal-ui-markdown-stream ui)
              (unless (getf markdown :finished-p) identifier))
        (when (or (getf previous :published-p) (plusp (length text)))
          (if (getf previous :published-p)
              (progn
                (clinedi:transcript-viewport-replace
                 (fullscreen-terminal-ui-viewport ui) (getf record :tag)
                 :text text :display display :regions regions)
                (terminal-ui-fullscreen--charge-markdown ui :all-p t))
              (progn
                (clinedi:transcript-viewport-append
                 (fullscreen-terminal-ui-viewport ui) text display
                 :regions regions :tag (getf record :tag))
                (terminal-ui-fullscreen--charge-markdown ui)))
          (setf (terminal-ui-fullscreen-welcome-p ui) nil
                (terminal-ui-compact-tail-group ui) nil))
        display))))

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
         ("drag" "to copy text"))
       '(("Ctrl-End" "to follow")))
   (when evicted-p
     '(("(history)" "earlier output")))))

(-> terminal-ui-fullscreen--legend-row
    (fullscreen-terminal-ui &key (:top integer) (:total integer) (:space integer)
                            (:width integer))
    string)
(defun terminal-ui-fullscreen--legend-row (ui &key top total space width)
  "Render the legend separating the transcript from the composer.

The position uses the legend accent, keys use the plain style, and descriptions
use the dim style. The modeline foregrounds are used without its background,
distinguishing the legend from the placeholder and the modeline."
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
  "Bound prompt-adjacent rows to LIMIT and retain visible widget hit regions."
  (let ((rows    nil)
        (actions nil))
    (dolist (row (terminal-ui--prompt-prefix-rows ui now))
      (let ((display (terminal-ui-fullscreen--display-rows ui (list row) width)))
        (when (and (listp row) (= (length display) 1))
          (loop for span in row
                for start = 1 then end
                for end = (+ start (terminal--spans-width (list span)))
                when (termdown:widget-p span)
                  do (push (list (length rows) start end (termdown:widget-action span))
                           actions)))
        (setf rows (append rows display))))
    (let* ((leading (position-if (lambda (row) (plusp (length row))) rows))
           (visible (and leading (nthcdr leading rows)))
           (count   (min limit (if visible (1+ (length visible)) 0))))
      (setf (fullscreen-terminal-ui-pinned-actions ui)
            (loop for (row start end action) in actions
                  when (and leading (<= leading row) (< row (+ leading count)))
                    collect (list (- row leading) start end action)))
      (subseq (append visible (when visible (list ""))) 0 count))))

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
        (dolist (region (fullscreen-terminal-ui-pinned-actions ui))
          (incf (first region) (+ space (if separator-p 1 0) 1)))
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
    (terminal-screen-paint (terminal-ui-terminal ui)
                           (fullscreen-terminal-ui-painter ui)
                           rows
                           :cursor-row       cursor-row
                           :cursor-column    cursor-column
                           :cursor-visible-p (and (fullscreen-terminal-ui-cursor-visible-p ui)
                                                  (not (terminal-ui-boot-waiting-p ui)))
                           :whole-screen-p   (terminal-ui-boot-waiting-p ui)))
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
  "Return the pinned widget or transcript action at one-based COLUMN and ROW."
  (or (loop for (action-row start end action) in (fullscreen-terminal-ui-pinned-actions ui)
            when (and (= row action-row) (<= start column) (< column end))
              return action)
      (multiple-value-bind (action text index)
          (clinedi:transcript-viewport-hit (fullscreen-terminal-ui-viewport ui) column row)
        (when index
          (or action
              (let ((url (url-at text index)))
                (and url (list ':open-url url))))))))

(-> terminal-ui-fullscreen--click (fullscreen-terminal-ui integer integer) boolean)
(defun terminal-ui-fullscreen--click (ui column row)
  "Queue the transcript action under COLUMN and ROW, reporting whether one exists.

The action runs after TERMINAL-UI-PROCESS-EVENT releases the presentation
lock, because copying and browser launches present notices of their own."
  (let ((action (terminal-ui-fullscreen--click-action ui column row)))
    (when action
      (setf (terminal-ui-pending-action ui) action))
    (not (null action))))

(-> terminal-ui-fullscreen--pointer (fullscreen-terminal-ui list) boolean)
(defun terminal-ui-fullscreen--pointer (ui event)
  "Select transcript text with a press, drag and release, or act on a plain click, as pointer EVENT says.

EVENT is (KIND COLUMN ROW) with KIND :CLICK, :DRAG or :RELEASE.

A press starts a selection. Dragging stretches it and paints it in reverse
video; releasing after a drag copies the text it covers. A release without a
drag is a click: the selection ends and the transcript action there runs."
  (destructuring-bind (kind column row) event
    (let ((viewport (fullscreen-terminal-ui-viewport ui)))
      (ecase kind
        (:click
         (clinedi:transcript-viewport-selection-start viewport column row)
         (setf (fullscreen-terminal-ui-press ui)     (list column row)
               (fullscreen-terminal-ui-dragged-p ui) nil)
         (terminal-ui--paint-live ui)
         t)
        (:drag
         (when (fullscreen-terminal-ui-press ui)
           (clinedi:transcript-viewport-selection-extend viewport column row)
           (setf (fullscreen-terminal-ui-dragged-p ui) t)
           (terminal-ui--paint-live ui))
         t)
        (:release
         (let* ((press (shiftf (fullscreen-terminal-ui-press ui) nil))
                (text  (and press
                            (shiftf (fullscreen-terminal-ui-dragged-p ui) nil)
                            (clinedi:transcript-viewport-selection-text viewport))))
           (cond
             (text
              (setf (terminal-ui-pending-action ui) (list ':copy text)))
             (t
              (clinedi:transcript-viewport-selection-clear viewport)
              (terminal-ui--paint-live ui)
              (when press
                (terminal-ui-fullscreen--click ui (first press) (second press)))))
           t))))))

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
      ((typep event '(cons (member :click :drag :release) (cons (integer 1) (cons (integer 1) null))))
       (terminal-ui-fullscreen--pointer ui event))
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

(defmethod terminal-ui--append-output :around
    ((ui fullscreen-terminal-ui) text display
     &key regions compact-first-group compact-last-group)
  "Queue unseparated output so deferred record batches keep their exact ordering."
  ;; Do not call the native-scrollback separator method: apply separators at publication.
  (terminal-ui-fullscreen--submit
   ui (list ':output text display regions compact-first-group compact-last-group)))

(-> terminal-ui-fullscreen--separate-output
    (fullscreen-terminal-ui list) (values string string list))
(defun terminal-ui-fullscreen--separate-output (ui operation)
  "Return OPERATION's text, display and regions with the current group separator."
  (destructuring-bind (kind text display regions first-group last-group) operation
    (declare (ignore kind))
    (let* ((previous (terminal-ui-compact-tail-group ui))
           (prefix   (if (and (plusp (length text))
                              (terminal-ui--separated-p previous first-group))
                         (string #\Newline)
                         "")))
      (when (plusp (length text))
        (unless (eq last-group ':attached)
          (setf (terminal-ui-compact-tail-group ui) last-group)))
      (values (concatenate 'string prefix text)
              (concatenate 'string prefix display)
              (termdown:shift-regions regions (length prefix))))))

(-> terminal-ui-fullscreen--submit (fullscreen-terminal-ui list) null)
(defun terminal-ui-fullscreen--submit (ui operation)
  "Queue OPERATION and publish once, retaining older queued work if publication fails."
  (let ((previous       (fullscreen-terminal-ui-deferred-operations ui))
        (previous-base  (fullscreen-terminal-ui-deferred-base-group ui))
        (previous-group (terminal-ui-compact-tail-group ui)))
    (unless previous
      (setf (fullscreen-terminal-ui-deferred-base-group ui) previous-group))
    (push operation (fullscreen-terminal-ui-deferred-operations ui))
    (handler-case
        (if (terminal-ui-live-output-suspended-p ui)
            (case (first operation)
              (:output
               (multiple-value-bind (text display regions)
                   (terminal-ui-fullscreen--separate-output ui operation)
                 (terminal-ui--defer-live-append ui text display :regions regions)))
              (:raw
               (terminal-ui--defer-live-append
                ui (second operation) (third operation) :regions (fourth operation))))
            (terminal-ui--present-live ui))
      (error (condition)
        (setf (fullscreen-terminal-ui-deferred-operations ui) previous
              (fullscreen-terminal-ui-deferred-base-group ui) previous-base
              (terminal-ui-compact-tail-group ui) previous-group)
        (error condition))))
  nil)

(-> terminal-ui-fullscreen--copy-publication-value (t) t)
(defun terminal-ui-fullscreen--copy-publication-value (value)
  "Copy mutable publication bookkeeping, retaining immutable record values."
  (typecase value
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key item) (setf (gethash key copy) item)) value)
       copy))
    (deque
     (deque-copy value))
    (t
     value)))

(-> terminal-ui-fullscreen--apply-operation (fullscreen-terminal-ui list) string)
(defun terminal-ui-fullscreen--apply-operation (ui operation)
  "Apply one queued OPERATION and return newly appended display text."
  (case (first operation)
    (:records
     (terminal-ui-fullscreen--apply-presentations ui (second operation)))
    (:output
     (multiple-value-bind (text display regions)
         (terminal-ui-fullscreen--separate-output ui operation)
       (terminal-ui-fullscreen--append ui text display :regions regions)
       display))
    (:raw
     (terminal-ui-fullscreen--append
      ui (second operation) (third operation) :regions (fourth operation))
     (third operation))
    (:markdown
     (terminal-ui-fullscreen--apply-markdown ui (second operation) (third operation)))
    (:tag
     (terminal-ui-fullscreen--tag-last ui (second operation))
     "")))

(defmethod terminal-ui--present-live
    ((ui fullscreen-terminal-ui) &key status-now (appended-text "") (appended-display "")
                                 appended-regions)
  "Publish ordered output and semantic replacements atomically with their bookkeeping."
  (cond
    ((plusp (length appended-text))
     (terminal-ui-fullscreen--submit
      ui (list ':raw appended-text appended-display appended-regions)))
    ((terminal-ui-live-output-suspended-p ui)
     nil)
    (t
     (let* ((terminal (terminal-ui-terminal ui))
            (saved    (loop for slot in '(presentation-records evicted-sequences
                                          finalized-identifiers finalized-order finalized-stamp
                                          compact-tail-group welcome-p deferred-operations
                                          stream-tail markdown-stream evicted-markdown-stream
                                          deferred-base-group deferred-live-appended-text
                                          deferred-live-appended-display deferred-live-appended-regions)
                            collect (cons slot (slot-value ui slot))))
            (done     nil))
       (let ((checkpoint (clinedi:transcript-viewport-checkpoint
                          (fullscreen-terminal-ui-viewport ui))))
         (unwind-protect
              (progn
                (dolist (binding saved)
                  (setf (slot-value ui (first binding))
                        (terminal-ui-fullscreen--copy-publication-value (rest binding))))
                (terminal-ui-fullscreen--ensure-width ui (max 1 (terminal-columns terminal)))
                (when (fullscreen-terminal-ui-deferred-operations ui)
                  (setf (terminal-ui-compact-tail-group ui)
                        (fullscreen-terminal-ui-deferred-base-group ui)))
                (let ((display
                        (with-output-to-string (output)
                          (dolist (operation (reverse (fullscreen-terminal-ui-deferred-operations ui)))
                            (write-string (terminal-ui-fullscreen--apply-operation ui operation) output)))))
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
                        (when (and (plusp (length display)) (terminal-plain-output-p terminal))
                          (terminal--write-safe-text terminal display)
                          (terminal-flush terminal)))))
                (setf (terminal-ui-deferred-live-appended-text ui) ""
                      (terminal-ui-deferred-live-appended-display ui) ""
                      (terminal-ui-deferred-live-appended-regions ui) nil
                      (fullscreen-terminal-ui-deferred-operations ui) nil
                      (fullscreen-terminal-ui-deferred-base-group ui) nil
                      done t))
           (unless done
             (clinedi:transcript-viewport-rollback (fullscreen-terminal-ui-viewport ui) checkpoint)
             (dolist (binding saved)
               (setf (slot-value ui (first binding)) (rest binding)))
             (terminal-ui-fullscreen-invalidate ui)))))))
  nil)
