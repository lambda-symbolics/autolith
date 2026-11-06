(in-package #:autolith)

;;;; -- Product Terminal Adapter --

(defclass localgroup-terminal (terminal image-daemon:relay)
  ((screen-active-p
    :initform      nil
    :accessor      localgroup-terminal-screen-active-p
    :type          boolean
    :documentation "Whether clients show whole-screen frames rather than streamed output.")
   (screen-frame
    :initform      nil
    :accessor      localgroup-terminal-screen-frame
    :type          (option vector)
    :documentation "The rows of the frame last sent, or NIL before the first complete frame.")
   (screen-width
    :initform      0
    :accessor      localgroup-terminal-screen-width
    :type          (integer 0)
    :documentation "The width the frame last sent was composed for.")
   (screen-cursor
    :initform      (list 0 0 t)
    :accessor      localgroup-terminal-screen-cursor
    :type          list
    :documentation "The row, column and visibility of the cursor in the frame last sent.")
   (screen-synced
    :initform      nil
    :accessor      localgroup-terminal-screen-synced
    :type          list
    :documentation "The attachments holding the frame last sent; the others get a complete one next."))
  (:documentation "An image-daemon relay integrated with Autolith terminal state.

Without a direct terminal, a fullscreen session's frames reach the clients
as rows rather than as painted output, so each client composes its own
screen around them."))

(-> localgroup-terminal-create
    (&key (:direct-terminal (option terminal)) (:rows (option integer))
          (:columns (option integer)) (:styled-p t))
    localgroup-terminal)
(defun localgroup-terminal-create (&key direct-terminal rows columns styled-p)
  "Create a relay terminal initially owned by DIRECT-TERMINAL when supplied.

Without one, use ROWS and COLUMNS, falling back to the startup client's
geometry and then terminal defaults. Enable styling if STYLED-P or the
startup record's :STYLED-P is non-NIL."
  (let ((startup-values
         (and *localgroup-startup-record* (rest *localgroup-startup-record*))))
    (make-instance 'localgroup-terminal :direct-terminal direct-terminal :rows
                   (if direct-terminal
                       (terminal-rows direct-terminal)
                       (or rows (getf startup-values :rows) *terminal-default-rows*))
                   :columns
                   (if direct-terminal
                       (terminal-columns direct-terminal)
                       (or columns (getf startup-values :columns) *terminal-default-columns*))
                   :interactive-p
                   (and direct-terminal (terminal-interactive-p direct-terminal))
                   :styled-p
                   (if direct-terminal
                       (terminal-styled-p direct-terminal)
                       (not (null (or styled-p (getf startup-values :styled-p))))))))

(-> localgroup-terminal-resize
    (localgroup-terminal &key (:rows integer) (:columns integer) (:styled-p t))
    null)
(defun localgroup-terminal-resize (terminal &key rows columns styled-p)
  "Adopt client styling and relay client dimensions to the interactive reader.

Dimensions are deliberately not applied here: writing them directly
desynchronizes the composed row width from the live-region geometry, and
every following repaint then eats one scrollback line while the live
region climbs toward the top of the screen. TERMINAL-UI-RESIZE is the
only dimension writer that keeps both in step and repaints."
  (with-lock-held ((image-daemon:relay-lock terminal))
    (setf (terminal-interactive-p terminal) t
          (terminal-styled-p terminal) (not (null styled-p))))
  (terminal-relayed-resize-publish terminal rows columns)
  nil)

(-> localgroup-terminal-switch-controller (localgroup-terminal string) boolean)
(defun localgroup-terminal-switch-controller (terminal identifier)
  "Ask TERMINAL's controlling client to move to the channel IDENTIFIER names.

Return whether a controller was attached to ask. The client detaches on its
own once it reads the request, so the relay keeps serving until then."
  (let ((controller (with-lock-held ((image-daemon:relay-lock terminal))
                      (image-daemon:relay-controller terminal))))
    (and controller
         (image-daemon:attachment-send controller (list :switch :session-id identifier)))))


;;;; -- Relayed Screen Frames --

(-> localgroup-terminal--attachments (localgroup-terminal) list)
(defun localgroup-terminal--attachments (terminal)
  "Return TERMINAL's controlling and observing attachments, under its relay lock."
  (remove-duplicates
   (append (when (image-daemon:relay-controller terminal)
             (list (image-daemon:relay-controller terminal)))
           (image-daemon:relay-observers terminal))
   :test #'eq))

(-> localgroup-terminal--frame-rows (vector) list)
(defun localgroup-terminal--frame-rows (frame)
  "Return FRAME's non-empty rows as (INDEX ROW) pairs."
  (loop for row across frame
        for index from 0
        unless (zerop (length row))
          collect (list index row)))

(-> localgroup-terminal--frame-packet (localgroup-terminal list boolean) list)
(defun localgroup-terminal--frame-packet (terminal rows complete-p)
  "Return the packet carrying ROWS of TERMINAL's screen, complete or as the rows that changed."
  (destructuring-bind (cursor-row cursor-column cursor-visible-p)
      (localgroup-terminal-screen-cursor terminal)
    (list :frame
          :height           (length (localgroup-terminal-screen-frame terminal))
          :width            (localgroup-terminal-screen-width terminal)
          :complete-p       complete-p
          :rows             rows
          :cursor-row       cursor-row
          :cursor-column    cursor-column
          :cursor-visible-p cursor-visible-p)))

(defmethod terminal-screen-enter ((terminal localgroup-terminal))
  "Start sending frames to TERMINAL's clients, or enter the screen of its direct terminal."
  (if (image-daemon:relay-direct-terminal terminal)
      (call-next-method)
      (with-lock-held ((image-daemon:relay-lock terminal))
        (setf (localgroup-terminal-screen-active-p terminal) t
              (localgroup-terminal-screen-frame terminal) nil
              (localgroup-terminal-screen-synced terminal) nil)
        nil)))

(defmethod terminal-screen-leave ((terminal localgroup-terminal) token)
  "Tell TERMINAL's clients to give their screens back, or leave the direct terminal's screen."
  (if (image-daemon:relay-direct-terminal terminal)
      (call-next-method)
      (progn
        (with-lock-held ((image-daemon:relay-lock terminal))
          (when (localgroup-terminal-screen-active-p terminal)
            (setf (localgroup-terminal-screen-active-p terminal) nil
                  (localgroup-terminal-screen-frame terminal) nil
                  (localgroup-terminal-screen-synced terminal) nil)
            (dolist (attachment (localgroup-terminal--attachments terminal))
              (image-daemon:attachment-send attachment (list :screen :leave)))))
        (platform-terminal-restore-fullscreen *platform* token)
        nil)))

(defmethod terminal-screen-paint
    ((terminal localgroup-terminal) painter rows
     &key (cursor-row 0) (cursor-column 0) (cursor-visible-p t))
  "Send TERMINAL's clients the rows that changed, and a complete frame to those without one."
  (if (image-daemon:relay-direct-terminal terminal)
      (call-next-method)
      (with-lock-held ((image-daemon:relay-lock terminal))
        (let* ((height      (max 1 (terminal-rows terminal)))
               (width       (max 1 (terminal-columns terminal)))
               (previous    (localgroup-terminal-screen-frame terminal))
               (frame       (make-array height :initial-element ""))
               (cursor      (list cursor-row cursor-column (not (null cursor-visible-p))))
               (attachments (localgroup-terminal--attachments terminal))
               (resized-p   (or (null previous)
                                (/= height (length previous))
                                (/= width (localgroup-terminal-screen-width terminal))))
               (moved-p     (not (equal cursor (localgroup-terminal-screen-cursor terminal)))))
          (loop for row in rows
                for index from 0 below height
                do (setf (aref frame index) row))
          (let ((changed (unless resized-p
                           (loop for index below height
                                 unless (string= (aref frame index) (aref previous index))
                                   collect (list index (aref frame index))))))
            (setf (localgroup-terminal-screen-frame terminal) frame
                  (localgroup-terminal-screen-width terminal) width
                  (localgroup-terminal-screen-cursor terminal) cursor
                  (localgroup-terminal-screen-synced terminal)
                  (remove-if-not (lambda (attachment) (member attachment attachments))
                                 (localgroup-terminal-screen-synced terminal)))
            (let ((complete (localgroup-terminal--frame-packet
                             terminal (localgroup-terminal--frame-rows frame) t))
                  (delta    (localgroup-terminal--frame-packet terminal changed nil)))
              (dolist (attachment attachments)
                (cond
                  ((or resized-p
                       (not (member attachment (localgroup-terminal-screen-synced terminal))))
                   (image-daemon:attachment-send attachment complete)
                   (pushnew attachment (localgroup-terminal-screen-synced terminal)))
                  ((or changed moved-p)
                   (image-daemon:attachment-send attachment delta)))))))
        nil)))

(defmethod terminal-screen-invalidate ((terminal localgroup-terminal) painter)
  "Send every client a complete frame next, or invalidate the direct terminal's painter."
  (if (image-daemon:relay-direct-terminal terminal)
      (call-next-method)
      (with-lock-held ((image-daemon:relay-lock terminal))
        (setf (localgroup-terminal-screen-frame terminal) nil
              (localgroup-terminal-screen-synced terminal) nil)
        nil)))

(-> localgroup-terminal-replay-screen (localgroup-terminal image-daemon:attachment) null)
(defun localgroup-terminal-replay-screen (terminal attachment)
  "Send ATTACHMENT the whole screen TERMINAL's clients show, when they show one it lacks."
  (with-lock-held ((image-daemon:relay-lock terminal))
    (let ((frame (localgroup-terminal-screen-frame terminal)))
      (when (and (localgroup-terminal-screen-active-p terminal)
                 frame
                 (not (member attachment (localgroup-terminal-screen-synced terminal))))
        (image-daemon:attachment-send
         attachment
         (localgroup-terminal--frame-packet terminal (localgroup-terminal--frame-rows frame) t))
        (push attachment (localgroup-terminal-screen-synced terminal)))))
  nil)


;;;; -- Transport Protocol --

(defmethod image-daemon:transport-rows ((terminal terminal))
  "Return TERMINAL's row count to the image-daemon relay."
  (terminal-rows terminal))

(defmethod (setf image-daemon:transport-rows) (value (terminal terminal))
  "Set TERMINAL's row count from the image-daemon relay."
  (setf (terminal-rows terminal) value))

(defmethod image-daemon:transport-columns ((terminal terminal))
  "Return TERMINAL's column count to the image-daemon relay."
  (terminal-columns terminal))

(defmethod (setf image-daemon:transport-columns) (value (terminal terminal))
  "Set TERMINAL's column count from the image-daemon relay."
  (setf (terminal-columns terminal) value))

(defmethod image-daemon:transport-started-p ((terminal terminal))
  "Return whether TERMINAL has been started."
  (terminal-started-p terminal))

(defmethod (setf image-daemon:transport-started-p) (value (terminal terminal))
  "Record whether TERMINAL has been started."
  (setf (terminal-started-p terminal) value))

(defmethod image-daemon:transport-interactive-p ((terminal terminal))
  "Return whether TERMINAL accepts interactive input."
  (terminal-interactive-p terminal))

(defmethod (setf image-daemon:transport-interactive-p) (value (terminal terminal))
  "Record whether TERMINAL accepts interactive input."
  (setf (terminal-interactive-p terminal) value))

(defmethod image-daemon:transport-styled-p ((terminal terminal))
  "Return whether TERMINAL renders styled output."
  (terminal-styled-p terminal))

(defmethod (setf image-daemon:transport-styled-p) (value (terminal terminal))
  "Record whether TERMINAL renders styled output."
  (setf (terminal-styled-p terminal) value))

(defmethod image-daemon:transport-start ((terminal stream-terminal))
  "Start the direct stream TERMINAL on behalf of the relay."
  (terminal-start terminal))

(defmethod terminal-start ((terminal localgroup-terminal))
  "Start the relay TERMINAL through the image-daemon transport protocol."
  (image-daemon:transport-start terminal))

(defmethod image-daemon:transport-stop ((terminal stream-terminal))
  "Stop the direct stream TERMINAL on behalf of the relay."
  (terminal-stop terminal))

(defmethod terminal-stop ((terminal localgroup-terminal))
  "Stop the relay TERMINAL through the image-daemon transport protocol."
  (image-daemon:transport-stop terminal))

(defmethod image-daemon:transport-write ((terminal stream-terminal) text)
  "Write TEXT to the direct stream TERMINAL on behalf of the relay."
  (terminal--write terminal text))

(defmethod terminal--write ((terminal localgroup-terminal) text)
  "Write TEXT through the relay TERMINAL to its direct terminal or client."
  (image-daemon:transport-write terminal text))

(defmethod image-daemon:transport-flush ((terminal stream-terminal))
  "Flush the direct stream TERMINAL on behalf of the relay."
  (terminal-flush terminal))

(defmethod terminal-plain-output-p ((terminal localgroup-terminal))
  "Defer to the direct terminal; without one the relay awaits a client.
The relay retains its output for the attaching client, which enters the
alternate screen and receives a complete repaint, so retained transcript text
would only spill into its normal buffer."
  (let ((direct (with-lock-held ((image-daemon:relay-lock terminal))
                  (image-daemon:relay-direct-terminal terminal))))
    (and direct (terminal-plain-output-p direct))))

(defmethod terminal-flush ((terminal localgroup-terminal))
  "Flush the relay TERMINAL through the image-daemon transport protocol."
  (image-daemon:transport-flush terminal))

(defmethod image-daemon:transport-input-ready-p ((terminal stream-terminal))
  "Return whether the direct stream TERMINAL has input ready."
  (terminal-input-ready-p terminal))

(defmethod terminal-input-ready-p ((terminal localgroup-terminal))
  "Return whether the relay TERMINAL has input ready from its owner."
  (image-daemon:transport-input-ready-p terminal))

(defmethod image-daemon:transport-read-event ((terminal stream-terminal))
  "Read one input event from the direct stream TERMINAL."
  (terminal-read-event terminal))

(defmethod terminal-read-event ((terminal localgroup-terminal))
  "Read one input event from the relay TERMINAL's current owner."
  (image-daemon:transport-read-event terminal))

(defmethod image-daemon:transport-set-dimensions
    ((terminal terminal) columns &key rows)
  "Resize TERMINAL to COLUMNS and, when supplied, ROWS."
  (terminal-set-dimensions terminal columns :rows rows))

(defmethod image-daemon:transport-resize
    ((terminal localgroup-terminal) &key rows columns styled-p)
  "Adopt a client's styling and relay its ROWS and COLUMNS to the interactive reader."
  (setf (terminal-interactive-p terminal) t
        (terminal-styled-p terminal) (not (null styled-p)))
  (terminal-relayed-resize-publish terminal rows columns)
  nil)
