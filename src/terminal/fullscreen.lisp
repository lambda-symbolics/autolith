(in-package #:autolith)

;;;; -- Fullscreen Viewport --

(defstruct (fullscreen-row (:constructor make-fullscreen-row (display chunk offset)))
  "A wrapped display row and its source anchor within a committed chunk."
  (display "" :type string)
  (chunk 0 :type (integer 0))
  (offset 0 :type (integer 0)))

(defclass fullscreen-terminal-ui (terminal-ui)
  ((chunks :initform (make-array 0 :adjustable t :fill-pointer 0)
           :reader fullscreen-terminal-ui-chunks
           :documentation "Unwrapped plain/display pairs retained for width reflow.")
   (rows :initform (make-array 0 :adjustable t :fill-pointer 0)
         :accessor fullscreen-terminal-ui-rows
         :documentation "Indexed wrapped transcript rows, extended only for new output.")
   (width :initform 0 :accessor fullscreen-terminal-ui-width
          :documentation "Width of the wrapped transcript cache.")
   (top :initform nil :accessor fullscreen-terminal-ui-top
        :type (option (integer 0))
        :documentation "Absolute first visible row, or NIL to follow the transcript tail.")
   (viewport-height :initform 1 :accessor fullscreen-terminal-ui-viewport-height
                    :documentation "Last painted transcript height, for page navigation.")
   (maximum-top :initform 0 :accessor fullscreen-terminal-ui-maximum-top
                :documentation "Last painted maximum scroll position.")
   (active-p :initform nil :accessor fullscreen-terminal-ui-active-p :type boolean
             :documentation "Whether this UI has entered the alternate buffer.")
   (platform-token :initform nil :accessor fullscreen-terminal-ui-platform-token
                   :documentation "Native output mode to restore after leaving the buffer.")
   (frame :initform nil :accessor fullscreen-terminal-ui-frame
          :documentation "Last successfully painted display-row vector, or NIL for full repaint.")
   (frame-width :initform 0 :accessor fullscreen-terminal-ui-frame-width
                :documentation "Width of the last successfully painted frame.")
   (cursor-visible-p :initform t :accessor fullscreen-terminal-ui-cursor-visible-p
                     :type boolean :documentation "Requested composer cursor visibility.")
   (welcome-tip :initform nil :accessor fullscreen-terminal-ui-welcome-tip
                :documentation "Startup advice retained across welcome repaints and resize.")
   (welcome-p :initform nil :accessor terminal-ui-fullscreen-welcome-p :type boolean
              :documentation "Whether the empty session shows the machine console panel."))
  (:documentation "An alternate-screen transcript viewport and bottom-pinned composer."))

(defmethod terminal-ui-fullscreen-p ((ui fullscreen-terminal-ui))
  "Use application-owned transcript scrolling for this UI."
  t)

(-> terminal-ui-fullscreen-invalidate (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-invalidate (ui)
  "Invalidate UI's physical frame without discarding its transcript or scroll anchor."
  (setf (fullscreen-terminal-ui-frame ui) nil)
  nil)

(-> terminal-ui-fullscreen-leave (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-leave (ui)
  "Leave only an owned alternate buffer and restore native state on every exit."
  (when (fullscreen-terminal-ui-active-p ui)
    (unwind-protect
         (progn
           (terminal--write
            (terminal-ui-terminal ui)
            (format nil "~C[0m~C[?7h~C[?25h~C[?1006l~C[?1000l~C[?1049l"
                    #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape))
           (terminal-flush (terminal-ui-terminal ui)))
      (let ((token (fullscreen-terminal-ui-platform-token ui)))
        (setf (fullscreen-terminal-ui-active-p ui) nil
              (fullscreen-terminal-ui-platform-token ui) nil)
        (terminal-ui-fullscreen-invalidate ui)
        (platform-terminal-restore-fullscreen *platform* token))))
  nil)

(-> terminal-ui-fullscreen-enter (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-enter (ui)
  "Enter the alternate buffer once, unwinding partial startup before propagating failure."
  (when (and (terminal-interactive-p (terminal-ui-terminal ui))
             (not (fullscreen-terminal-ui-active-p ui)))
    (let ((token (platform-terminal-enable-fullscreen *platform*))
          (completed-p nil))
      (unwind-protect
           (progn
             (setf (fullscreen-terminal-ui-platform-token ui) token
                   (fullscreen-terminal-ui-active-p ui) t)
             (terminal--write
              (terminal-ui-terminal ui)
              (format nil "~C[?1049h~C[2J~C[H~C[?1000h~C[?1006h"
                      #\Escape #\Escape #\Escape #\Escape #\Escape))
             (terminal-flush (terminal-ui-terminal ui))
             (terminal-ui-fullscreen-invalidate ui)
             (setf completed-p t))
        (unless completed-p
          (ignore-errors (terminal-ui-fullscreen-leave ui))))))
  nil)


;;;; -- Incremental Transcript and Reflow --

(-> terminal-ui-fullscreen--wrap-chunk (list integer integer) list)
(defun terminal-ui-fullscreen--wrap-chunk (chunk index width)
  "Wrap one original CHUNK, retaining character anchors for later width changes."
  (destructuring-bind (text display) chunk
    (let* ((pairs (clinedi:wrap-styled-text text display width))
           (offset 0))
      ;; A final newline begins the next append, rather than an extra empty row.
      (when (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline))
        (setf pairs (butlast pairs)))
      (loop for (plain styled) in pairs
            for start = (or (search plain text :start2 offset) offset)
            collect (make-fullscreen-row styled index start)
            do (setf offset (+ start (length plain)))
               (when (and (< offset (length text)) (char= (char text offset) #\Newline))
                 (incf offset))))))

(-> terminal-ui-fullscreen--ensure-width (fullscreen-terminal-ui integer) null)
(defun terminal-ui-fullscreen--ensure-width (ui width)
  "Reflow original chunks only after a width change, preserving the visible source anchor."
  (unless (= width (fullscreen-terminal-ui-width ui))
    (let* ((old (fullscreen-terminal-ui-rows ui))
           (top (fullscreen-terminal-ui-top ui))
           (anchor (and top (< top (length old)) (aref old top)))
           (rows (make-array 0 :adjustable t :fill-pointer 0))
           (new-top nil))
      (loop for chunk across (fullscreen-terminal-ui-chunks ui)
            for index from 0
            do (dolist (row (terminal-ui-fullscreen--wrap-chunk chunk index width))
                 (when (and anchor (= index (fullscreen-row-chunk anchor))
                            (<= (fullscreen-row-offset row) (fullscreen-row-offset anchor)))
                   (setf new-top (length rows)))
                 (vector-push-extend row rows)))
      (setf (fullscreen-terminal-ui-rows ui) rows
            (fullscreen-terminal-ui-width ui) width
            (fullscreen-terminal-ui-top ui) (and top (or new-top top)))
      (terminal-ui-fullscreen-invalidate ui)))
  nil)

(-> terminal-ui-fullscreen--append (fullscreen-terminal-ui string string) null)
(defun terminal-ui-fullscreen--append (ui text display)
  "Extend the transcript cache with one committed output chunk."
  (when (plusp (length text))
    (let* ((chunks (fullscreen-terminal-ui-chunks ui))
           (index (length chunks))
           (chunk (list text display))
           (rows (terminal-ui-fullscreen--wrap-chunk
                  chunk index (fullscreen-terminal-ui-width ui))))
      (vector-push-extend chunk chunks)
      (dolist (row rows)
        (vector-push-extend row (fullscreen-terminal-ui-rows ui)))
      (setf (terminal-ui-fullscreen-welcome-p ui) nil)))
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
      (multiple-value-bind (wrapped styled position)
          (clinedi:wrap-styled-editor-text text display :cursor cursor :columns width)
        (let* ((lines (mapcar #'second (clinedi:wrap-styled-text wrapped styled width)))
               (limit (max 1 (if (terminal-ui-selector ui)
                                 (- height 2)
                                 (floor height 2)))))
          (multiple-value-bind (row column pending-wrap)
              (clinedi:screen-position wrapped :columns width :end position)
            (declare (ignore pending-wrap))
            (when (>= row (length lines))
              (setf lines (append lines (make-list (1+ (- row (length lines)))
                                                  :initial-element ""))))
            (let* ((count (min limit (length lines)))
                   (start (min (max 0 (- row (1- count)))
                               (max 0 (- (length lines) count)))))
              (values (subseq lines start (+ start count)) (- row start) column))))))))

(-> terminal-ui-fullscreen--frame (fullscreen-terminal-ui (option real))
    (values list integer integer))
(defun terminal-ui-fullscreen--frame (ui status-now)
  "Compose only visible transcript rows above a fixed-bottom composer."
  (let* ((terminal (terminal-ui-terminal ui))
         (width (max 1 (terminal-columns terminal)))
         (height (max 1 (terminal-rows terminal))))
    (terminal-ui-fullscreen--ensure-width ui width)
    (multiple-value-bind (composer cursor-row cursor-column)
        (terminal-ui-fullscreen--composer ui width height)
      (let* ((separator-p (> height (length composer)))
             (space (max 0 (- height (length composer) (if separator-p 1 0))))
             (committed (fullscreen-terminal-ui-rows ui))
             (live (coerce (terminal-ui-fullscreen--display-rows
                            ui (terminal-ui--live-prefix-rows ui status-now) width) 'vector))
             (total (+ (length committed) (length live)))
             (maximum-top (max 0 (- total space)))
             (top (min (or (fullscreen-terminal-ui-top ui) maximum-top) maximum-top))
             (visible
               (if (and (zerop total) (terminal-ui-fullscreen-welcome-p ui))
                   (terminal-ui--welcome-rows ui space)
                   (loop for index from top below (min total (+ top space))
                         collect (if (< index (length committed))
                                     (fullscreen-row-display (aref committed index))
                                     (aref live (- index (length committed))))))))
        (setf (fullscreen-terminal-ui-viewport-height ui) space
              (fullscreen-terminal-ui-maximum-top ui) maximum-top)
        (when (fullscreen-terminal-ui-top ui)
          (setf (fullscreen-terminal-ui-top ui) top))
        (values
         (append visible (make-list (max 0 (- space (length visible))) :initial-element "")
                 (when separator-p
                   (list (terminal--render-spans
                          terminal
                          (list (terminal-span
                                 ':hint
                                 (layout-fit-text
                                  (if (fullscreen-terminal-ui-top ui)
                                      (format nil "[ ~D-~D / ~D ]  PgUp/PgDn scroll . Ctrl-End follows"
                                              (1+ top) (min total (+ top space)) total)
                                      "[ LIVE ]  PgUp/PgDn scroll . Ctrl-Home/End . Shift: select")
                                  width))))))
                 composer)
         (+ space (if separator-p 1 0) cursor-row) cursor-column)))))


;;;; -- Physical Frames and Navigation --

(-> terminal-ui-fullscreen-paint
    (fullscreen-terminal-ui &key (:rows list) (:cursor-row integer) (:cursor-column integer)) null)
(defun terminal-ui-fullscreen-paint (ui &key rows (cursor-row 0) (cursor-column 0))
  "Diff trusted display-string ROWS at absolute positions, without scrolling the terminal."
  (when (fullscreen-terminal-ui-active-p ui)
    (let* ((terminal (terminal-ui-terminal ui))
           (height (max 1 (terminal-rows terminal)))
           (width (max 1 (terminal-columns terminal)))
           (frame (make-array height :initial-element ""))
           (previous (fullscreen-terminal-ui-frame ui))
           (complete-p (or (null previous) (/= height (length previous))
                           (/= width (fullscreen-terminal-ui-frame-width ui)))))
      (loop for row in rows for index from 0 below height
            do (setf (aref frame index)
                     (second (first (clinedi:wrap-styled-text
                                     (clinedi:ansi-strip row) row width)))))
      (handler-case
          (progn
            (terminal--write
             terminal
             (with-output-to-string (output)
               (format output "~C[?25l~C[?7l" #\Escape #\Escape)
               (loop for row across frame for index from 0
                     when (or complete-p (not (string= row (aref previous index))))
                       do (format output "~C[~D;1H~C[0m~C]8;;~C\\~C[2K~A~C]8;;~C\\~C[0m"
                                  #\Escape (1+ index) #\Escape #\Escape #\Escape #\Escape row
                                  #\Escape #\Escape #\Escape))
               (format output "~C[~D;~DH~C[?7h~C[?25~A"
                       #\Escape (1+ (max 0 (min cursor-row (1- height))))
                       (1+ (max 0 (min cursor-column (1- width))))
                       #\Escape #\Escape
                       (if (fullscreen-terminal-ui-cursor-visible-p ui) "h" "l"))))
            (terminal-flush terminal)
            (setf (fullscreen-terminal-ui-frame ui) frame
                  (fullscreen-terminal-ui-frame-width ui) width))
        (error (condition)
          (terminal-ui-fullscreen-invalidate ui)
          (error condition)))))
  nil)

(-> terminal-ui-fullscreen-scroll (fullscreen-terminal-ui integer) null)
(defun terminal-ui-fullscreen-scroll (ui delta)
  "Move DELTA rows through history; positive values move towards the latest output."
  (let* ((maximum (fullscreen-terminal-ui-maximum-top ui))
         (top (max 0 (min maximum (+ (or (fullscreen-terminal-ui-top ui) maximum) delta)))))
    (setf (fullscreen-terminal-ui-top ui) (unless (= top maximum) top))
    (terminal-ui--paint-live ui))
  nil)

(-> terminal-ui-fullscreen-bottom (fullscreen-terminal-ui) null)
(defun terminal-ui-fullscreen-bottom (ui)
  "Follow the latest transcript output again."
  (setf (fullscreen-terminal-ui-top ui) nil)
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui-fullscreen-handle-event (fullscreen-terminal-ui t) boolean)
(defun terminal-ui-fullscreen-handle-event (ui event)
  "Consume viewport navigation without changing the draft or its history position."
  (block nil
    (cond
      ((eq event ':page-up)
       (terminal-ui-fullscreen-scroll ui (- (max 1 (1- (fullscreen-terminal-ui-viewport-height ui))))))
      ((eq event ':page-down)
       (terminal-ui-fullscreen-scroll ui (max 1 (1- (fullscreen-terminal-ui-viewport-height ui)))))
      ((eq event ':scroll-top)
       (setf (fullscreen-terminal-ui-top ui) 0)
       (terminal-ui--paint-live ui))
      ((eq event ':scroll-bottom)
       (terminal-ui-fullscreen-bottom ui))
      ((and (consp event) (eq (first event) ':scroll) (member (second event) '(-1 1)))
       (terminal-ui-fullscreen-scroll ui (* 3 (second event))))
      ((eq event ':clear-screen)
       (terminal-ui-fullscreen-invalidate ui)
       (terminal-ui--paint-live ui))
      (t
       (when (and (consp event) (member (first event) '(:insert :paste :line)))
         (setf (terminal-ui-fullscreen-welcome-p ui) nil))
       (return nil)))
    t))


;;;; -- Presentation Transactions --

(defmethod terminal-ui--append-output ((ui fullscreen-terminal-ui) text display)
  "Commit output through the same transaction as streamed live presentation."
  (terminal-ui--present-live ui :appended-text text :appended-display display))

(defmethod terminal-ui--present-live
    ((ui fullscreen-terminal-ui) &key status-now (appended-text "") (appended-display ""))
  "Append, paint and commit atomically; a failed paint can be retried without duplication."
  (if (terminal-ui-live-output-suspended-p ui)
      (terminal-ui--defer-live-append ui appended-text appended-display)
      (let* ((terminal (terminal-ui-terminal ui))
             (text (concatenate 'string (terminal-ui-deferred-live-appended-text ui) appended-text))
             (display (concatenate 'string (terminal-ui-deferred-live-appended-display ui) appended-display)))
        (terminal-ui-fullscreen--ensure-width ui (max 1 (terminal-columns terminal)))
        (let ((chunk-count (length (fullscreen-terminal-ui-chunks ui)))
              (row-count (length (fullscreen-terminal-ui-rows ui)))
              (top (fullscreen-terminal-ui-top ui))
              (welcome-p (terminal-ui-fullscreen-welcome-p ui))
              (completed-p nil))
          (unwind-protect
               (progn
                 (terminal-ui-fullscreen--append ui text display)
                 (when (terminal-ui-started-p ui)
                   (if (terminal-interactive-p terminal)
                       (progn
                         (terminal-ui-fullscreen-enter ui)
                         (let ((now (or status-now (funcall (terminal-ui-clock-function ui)))))
                           (terminal-ui--expire-notice-at ui now)
                           (setf (terminal-ui-status-rendered-signature ui)
                                 (terminal-ui--animation-signature-at ui now))
                           (multiple-value-bind (rows cursor-row cursor-column)
                               (terminal-ui-fullscreen--frame ui now)
                             (terminal-ui-fullscreen-paint
                              ui :rows rows :cursor-row cursor-row :cursor-column cursor-column))
                           (terminal-ui--note-command-paint ui (terminal-ui--command-visible-activities ui))))
                       (when (plusp (length display))
                         (terminal--write-safe-text terminal display)
                         (terminal-flush terminal))))
                 (setf (terminal-ui-deferred-live-appended-text ui) ""
                       (terminal-ui-deferred-live-appended-display ui) ""
                       completed-p t))
            (unless completed-p
              (setf (fill-pointer (fullscreen-terminal-ui-chunks ui)) chunk-count
                    (fill-pointer (fullscreen-terminal-ui-rows ui)) row-count
                    (fullscreen-terminal-ui-top ui) top
                    (terminal-ui-fullscreen-welcome-p ui) welcome-p)
              (terminal-ui-fullscreen-invalidate ui))))))
  nil)


;;;; -- Remote Client Ownership --

(defclass fullscreen-output-stream (sb-gray:fundamental-character-output-stream)
  ((output :initarg :output :reader fullscreen-output-stream-output
           :documentation "Client output destination.")
   (prefix :initform 0 :accessor fullscreen-output-stream-prefix
           :documentation "Matched characters of the alternate-buffer control prefix.")
   (active-p :initform nil :accessor fullscreen-output-stream-active-p
             :documentation "Whether remote output entered the client's alternate buffer."))
  (:documentation "Forward remote output while tracking alternate-buffer ownership across packets."))

(-> fullscreen-output-stream--track (fullscreen-output-stream character) null)
(defun fullscreen-output-stream--track (stream character)
  "Recognize complete alternate-buffer controls without retaining remote text."
  (let* ((prefix (load-time-value (format nil "~C[?1049" #\Escape) t))
         (position (fullscreen-output-stream-prefix stream)))
    (cond
      ((= position (length prefix))
       (case character
         (#\h (setf (fullscreen-output-stream-active-p stream) t))
         (#\l (setf (fullscreen-output-stream-active-p stream) nil)))
       (setf (fullscreen-output-stream-prefix stream) (if (char= character #\Escape) 1 0)))
      ((char= character (char prefix position))
       (incf (fullscreen-output-stream-prefix stream)))
      (t
       (setf (fullscreen-output-stream-prefix stream) (if (char= character #\Escape) 1 0)))))
  nil)

(defmethod sb-gray:stream-write-char ((stream fullscreen-output-stream) character)
  "Track control state before forwarding CHARACTER to the client."
  (fullscreen-output-stream--track stream character)
  (write-char character (fullscreen-output-stream-output stream)))

(defmethod sb-gray:stream-write-string ((stream fullscreen-output-stream) string &optional (start 0) end)
  "Track controls even when transport packets split an escape sequence."
  (loop for index from start below (or end (length string))
        do (fullscreen-output-stream--track stream (char string index)))
  (write-string string (fullscreen-output-stream-output stream) :start start :end end))

(defmethod sb-gray:stream-finish-output ((stream fullscreen-output-stream))
  "Flush the client's output destination."
  (finish-output (fullscreen-output-stream-output stream)))

(defmethod sb-gray:stream-force-output ((stream fullscreen-output-stream))
  "Flush the client's output destination."
  (force-output (fullscreen-output-stream-output stream)))

(-> fullscreen-output-stream-restore (fullscreen-output-stream) null)
(defun fullscreen-output-stream-restore (stream)
  "Restore an owned client buffer after detach, revocation or connection loss."
  (when (fullscreen-output-stream-active-p stream)
    (setf (fullscreen-output-stream-active-p stream) nil)
    (write-string (format nil "~C[0m~C[?7h~C[?25h~C[?1006l~C[?1000l~C[?1049l"
                          #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape)
                  (fullscreen-output-stream-output stream))
    (finish-output (fullscreen-output-stream-output stream)))
  nil)
