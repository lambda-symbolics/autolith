(in-package #:autolith)

;;;; -- Client Screen --

;;; A terminal client of a hosted fullscreen session receives the session's
;;; screen as frames of display rows rather than as painted output, and
;;; paints them on its own terminal. The client therefore owns the alternate
;;; buffer, the frame painter and the cursor, and composes its own side
;;; panel beside the session's rows.

(defparameter *localgroup-screen-dimension-limit* 10000
  "The largest frame height or width a client accepts from a session.")

(defparameter *localgroup-screen-toggle-event* '(:control #\t)
  "The key event showing or hiding the side panel.")


;;;; -- Side Panels --

(defgeneric screen-panel-width (panel columns)
  (:documentation "Return the columns PANEL takes left of the session's rows on a terminal COLUMNS wide."))

(defgeneric screen-panel-minimum-columns (panel)
  (:documentation "Return the narrowest terminal that shows PANEL beside the session; a narrower one shows it alone."))

(defgeneric screen-panel-refresh (panel)
  (:documentation "Gather PANEL's current content and return whether it changed since the last call."))

(defgeneric screen-panel-rows (panel terminal &key height width)
  (:documentation "Return HEIGHT display rows of PANEL rendered for TERMINAL, each exactly WIDTH cells."))

(defgeneric screen-panel-click (panel column row)
  (:documentation "Act on a click at COLUMN and ROW of PANEL, both from 1.

Return :REDRAW when PANEL changed and wants painting, a (:SWITCH :SESSION-ID
ID) request to move the terminal to channel ID, or NIL."))

(defgeneric screen-panel-shown-p (panel)
  (:documentation "Return PANEL's visibility preference, or :SCREEN to use the screen's.

Specialize this and its setter to retain the choice across reconnects and
switches. The default methods leave the choice local to each screen."))

(defgeneric (setf screen-panel-shown-p) (shown-p panel)
  (:documentation "Record whether the person wants PANEL beside the session."))

(defmethod screen-panel-click ((panel t) column row)
  "A panel without clickable rows ignores clicks."
  (declare (ignore column row))
  nil)

(defmethod screen-panel-shown-p ((panel t))
  "A panel without a memory of its own leaves the choice to the screen."
  ':screen)

(defmethod (setf screen-panel-shown-p) (shown-p (panel t))
  "A panel without a memory of its own leaves the choice to the screen."
  shown-p)


;;;; -- Screens --

(defclass localgroup-screen ()
  ((terminal
    :initarg       :terminal
    :reader        localgroup-screen-terminal
    :type          terminal
    :documentation "The client's own terminal, painted directly.")
   (painter
    :initform      (clinedi:make-frame-painter)
    :reader        localgroup-screen-painter
    :documentation "Paints frames by rewriting the rows that changed since the last one.")
   (lock
    :initform      (make-lock "Autolith client screen")
    :reader        localgroup-screen-lock
    :documentation "The lock serializing frame arrival and painting across threads.")
   (token
    :initform      nil
    :accessor      localgroup-screen-token
    :documentation "The native mode token captured when the screen was taken.")
   (active-p
    :initform      nil
    :accessor      localgroup-screen-active-p
    :type          boolean
    :documentation "Whether the client shows frames in its alternate buffer.")
   (frame
    :initform      nil
    :accessor      localgroup-screen-frame
    :type          (option vector)
    :documentation "The session's rows as last received, or NIL before a complete frame.")
   (cursor
    :initform      (list 0 0 t)
    :accessor      localgroup-screen-cursor
    :type          list
    :documentation "The cursor row, column and visibility of the frame.")
   (whole-screen-p
    :initform      nil
    :accessor      localgroup-screen-whole-screen-p
    :type          boolean
    :documentation "Whether the session's frame asks for the whole screen, hiding the side panel.")
   (panel
    :initarg       :panel
    :initform      nil
    :reader        localgroup-screen-panel
    :documentation "The side panel composed left of the session's rows, or NIL.")
   (own-panel-shown-p
    :initform      t
    :accessor      localgroup-screen-own-panel-shown-p
    :type          boolean
    :documentation "Whether the person wants the side panel, for a panel that keeps no choice of its own.")
   (resize-pending-p
    :initform      nil
    :accessor      localgroup-screen-resize-pending-p
    :type          boolean
    :documentation "Whether the session's pane changed size since the client last told the session."))
  (:documentation "The whole screen a client paints from a hosted session's frames."))

(-> localgroup-screen-create (&key (:terminal terminal) (:panel t)) localgroup-screen)
(defun localgroup-screen-create (&key terminal panel)
  "Return a screen painting a session's frames on the client's TERMINAL beside PANEL."
  (make-instance 'localgroup-screen :terminal terminal :panel panel))

(-> localgroup-screen-panel-shown-p (localgroup-screen) boolean)
(defun localgroup-screen-panel-shown-p (screen)
  "Return whether the person wants SCREEN's side panel: the panel's own memory, else the screen's."
  (let* ((panel (localgroup-screen-panel screen))
         (kept  (and panel (screen-panel-shown-p panel))))
    (if (eq kept ':screen)
        (localgroup-screen-own-panel-shown-p screen)
        (and kept t))))

(-> localgroup-screen-layout (localgroup-screen) (member :session :beside :alone))
(defun localgroup-screen-layout (screen)
  "Return how SCREEN shares its terminal: the session alone, the side panel beside it, or the panel alone.

The panel shows when the person wants it and the session's frame does not ask
for the whole screen. A terminal narrower than the panel's minimum gives it
the whole screen; the person hides it again to see the session."
  (let ((panel (localgroup-screen-panel screen)))
    (cond
      ((or (null panel)
           (localgroup-screen-whole-screen-p screen)
           (not (localgroup-screen-panel-shown-p screen)))
       ':session)
      ((< (terminal-columns (localgroup-screen-terminal screen))
          (screen-panel-minimum-columns panel))
       ':alone)
      (t
       ':beside))))

(-> localgroup-screen-panel-visible-p (localgroup-screen) boolean)
(defun localgroup-screen-panel-visible-p (screen)
  "Return true when SCREEN's side panel takes part or all of the terminal."
  (not (eq (localgroup-screen-layout screen) ':session)))

(-> localgroup-screen--panel-width (localgroup-screen) (integer 0))
(defun localgroup-screen--panel-width (screen)
  "Return the columns SCREEN's side panel takes: none, its share beside the session, or all."
  (let ((columns (terminal-columns (localgroup-screen-terminal screen))))
    (ecase (localgroup-screen-layout screen)
      (:session
       0)
      (:beside
       (screen-panel-width (localgroup-screen-panel screen) columns))
      (:alone
       columns))))

(-> localgroup-screen-pane-size (localgroup-screen) (values integer integer))
(defun localgroup-screen-pane-size (screen)
  "Return the rows and columns the session's pane gets of SCREEN's terminal.

Beside the panel it gets the columns the panel and its rule leave. Behind a
panel that takes the whole screen it keeps the whole width, so hiding the
panel shows it without a resize."
  (let ((terminal (localgroup-screen-terminal screen)))
    (values (terminal-rows terminal)
            (if (eq (localgroup-screen-layout screen) ':beside)
                (max 1 (- (terminal-columns terminal) (localgroup-screen--panel-width screen) 1))
                (terminal-columns terminal)))))

(-> localgroup-screen-take-resize (localgroup-screen) boolean)
(defun localgroup-screen-take-resize (screen)
  "Return whether the pane changed size since the last call, clearing the note."
  (with-lock-held ((localgroup-screen-lock screen))
    (shiftf (localgroup-screen-resize-pending-p screen) nil)))

(-> localgroup-screen-filter-event (localgroup-screen t) t)
(defun localgroup-screen-filter-event (screen event)
  "Return EVENT as the session should see it, :IGNORE when SCREEN consumed it, or a switch request.

The toggle key shows or hides the side panel. A click lands in the panel,
which acts on it and may ask to move the terminal to another channel with a
(:SWITCH :SESSION-ID ID) request, or in the session's pane, whose columns
start after the panel. While the panel takes the whole screen, the hidden
session gets no keys, and a switch hides the panel to show the channel
switched to."
  (let ((click-p (typep event '(cons (eql :click) (cons integer (cons integer null))))))
    (cond
      ((equal event *localgroup-screen-toggle-event*)
       (localgroup-screen-toggle-panel screen)
       ':ignore)
      ((eq (localgroup-screen-layout screen) ':alone)
       (cond
         ((member event '(:interrupt :end-of-input :stream-end))
          event)
         (click-p
          (let ((result (localgroup-screen--panel-click screen (second event) (third event))))
            (when (consp result)
              (localgroup-screen-toggle-panel screen))
            result))
         (t
          ':ignore)))
      ((and click-p (eq (localgroup-screen-layout screen) ':beside))
       (let ((offset (1+ (localgroup-screen--panel-width screen))))
         (if (<= (second event) offset)
             (localgroup-screen--panel-click screen (second event) (third event))
             (list ':click (- (second event) offset) (third event)))))
      (t
       event))))

(-> localgroup-screen--panel-click (localgroup-screen integer integer) t)
(defun localgroup-screen--panel-click (screen column row)
  "Let SCREEN's panel act on a click at COLUMN and ROW; return its switch request or :IGNORE."
  (let ((result (screen-panel-click (localgroup-screen-panel screen) column row)))
    (cond
      ((eq result ':redraw)
       (with-lock-held ((localgroup-screen-lock screen))
         (localgroup-screen--paint screen))
       ':ignore)
      ((consp result)
       result)
      (t
       ':ignore))))

(-> localgroup-screen-toggle-panel (localgroup-screen) null)
(defun localgroup-screen-toggle-panel (screen)
  "Toggle SCREEN's side panel and report any pane-size change to the session.

Store the choice on the screen and, if supported, on the panel for reuse
across attachments."
  (let ((panel (localgroup-screen-panel screen)))
    (when panel
      (with-lock-held ((localgroup-screen-lock screen))
        (let ((layout   (localgroup-screen-layout screen))
              (columns  (nth-value 1 (localgroup-screen-pane-size screen)))
              (wanted-p (not (localgroup-screen-panel-shown-p screen))))
          (setf (screen-panel-shown-p panel) wanted-p
                (localgroup-screen-own-panel-shown-p screen) wanted-p)
          (unless (eq layout (localgroup-screen-layout screen))
            (unless (= columns (nth-value 1 (localgroup-screen-pane-size screen)))
              (setf (localgroup-screen-resize-pending-p screen) t))
            (terminal-screen-invalidate (localgroup-screen-terminal screen)
                                        (localgroup-screen-painter screen))
            (localgroup-screen--paint screen))))))
  nil)

(-> localgroup-screen-tick (localgroup-screen) null)
(defun localgroup-screen-tick (screen)
  "Refresh SCREEN's side panel and repaint when its content changed."
  (let ((panel (localgroup-screen-panel screen)))
    (when (and panel
               (localgroup-screen-active-p screen)
               (localgroup-screen-panel-visible-p screen)
               (screen-panel-refresh panel))
      (with-lock-held ((localgroup-screen-lock screen))
        (localgroup-screen--paint screen))))
  nil)

(-> localgroup-screen--frame-row-p (t) boolean)
(defun localgroup-screen--frame-row-p (row)
  "Return true when ROW is an (INDEX TEXT) pair of a frame."
  (and (consp row)
       (consp (rest row))
       (null (rest (rest row)))
       (typep (first row) '(integer 0))
       (stringp (second row))))

(-> localgroup-screen--frame-packet-p (t) boolean)
(defun localgroup-screen--frame-packet-p (packet)
  "Return true when PACKET is a well-formed frame."
  (and (consp packet)
       (eq (first packet) ':frame)
       (let ((properties (rest packet)))
         (and (listp properties)
              (handler-case (evenp (list-length properties))
                (type-error ()
                  nil))
              (typep (getf properties :height)
                     `(integer 1 ,*localgroup-screen-dimension-limit*))
              (typep (getf properties :width)
                     `(integer 1 ,*localgroup-screen-dimension-limit*))
              (typep (getf properties :cursor-row) '(integer 0))
              (typep (getf properties :cursor-column) '(integer 0))
              (let ((rows (getf properties :rows)))
                (and (listp rows)
                     (handler-case (not (null (list-length rows)))
                       (type-error ()
                         nil))
                     (every #'localgroup-screen--frame-row-p rows)))))))

(-> localgroup-screen-handle-packet (localgroup-screen t) boolean)
(defun localgroup-screen-handle-packet (screen packet)
  "Apply a frame or screen PACKET from the session and return whether it was one.

A complete frame takes the screen when the client does not show one yet. A
frame of changed rows arriving before any complete frame is dropped, since
there is nothing to change; the session sends a complete frame to every
client it has not synchronized."
  (cond
    ((localgroup-screen--frame-packet-p packet)
     (with-lock-held ((localgroup-screen-lock screen))
       (when (localgroup-screen--apply-frame screen (rest packet))
         (localgroup-screen--paint screen)))
     t)
    ((equal packet '(:screen :leave))
     (with-lock-held ((localgroup-screen-lock screen))
       (localgroup-screen--leave screen))
     t)
    (t
     nil)))

(-> localgroup-screen-resized (localgroup-screen) null)
(defun localgroup-screen-resized (screen)
  "Repaint SCREEN completely after the client's terminal changed size."
  (with-lock-held ((localgroup-screen-lock screen))
    (terminal-screen-invalidate (localgroup-screen-terminal screen)
                                (localgroup-screen-painter screen))
    (localgroup-screen--paint screen))
  nil)

(-> localgroup-screen-close (localgroup-screen) null)
(defun localgroup-screen-close (screen)
  "Give SCREEN's terminal back when the client still shows a session's frames."
  (with-lock-held ((localgroup-screen-lock screen))
    (localgroup-screen--leave screen))
  nil)

(-> localgroup-screen--apply-frame (localgroup-screen list) boolean)
(defun localgroup-screen--apply-frame (screen properties)
  "Record the frame PROPERTIES describe and return whether there is a frame to paint."
  (let* ((height     (getf properties :height))
         (complete-p (not (null (getf properties :complete-p))))
         (previous   (localgroup-screen-frame screen))
         (frame      (cond
                       (complete-p
                        (make-array height :initial-element ""))
                       ((and previous (= (length previous) height))
                        (copy-seq previous))
                       (t
                        nil))))
    (when frame
      (dolist (row (getf properties :rows))
        (when (< (first row) height)
          (setf (aref frame (first row)) (second row))))
      (localgroup-screen--take-whole-screen screen (not (null (getf properties :whole-screen-p))))
      (setf (localgroup-screen-frame screen) frame
            (localgroup-screen-cursor screen)
            (list (getf properties :cursor-row)
                  (getf properties :cursor-column)
                  (not (null (getf properties :cursor-visible-p)))))
      (unless (localgroup-screen-active-p screen)
        (localgroup-screen--enter screen)))
    (not (null frame))))

(-> localgroup-screen--take-whole-screen (localgroup-screen boolean) null)
(defun localgroup-screen--take-whole-screen (screen whole-screen-p)
  "Record whether the session wants the whole screen, as WHOLE-SCREEN-P says.

When that shows or hides the side panel, the session's pane changes size: the
client tells the session, and the next paint rewrites the whole screen."
  (let ((visible-p (localgroup-screen-panel-visible-p screen)))
    (setf (localgroup-screen-whole-screen-p screen) whole-screen-p)
    (unless (eq visible-p (localgroup-screen-panel-visible-p screen))
      (setf (localgroup-screen-resize-pending-p screen) t)
      (terminal-screen-invalidate (localgroup-screen-terminal screen)
                                  (localgroup-screen-painter screen))))
  nil)

(-> localgroup-screen--enter (localgroup-screen) null)
(defun localgroup-screen--enter (screen)
  "Take the client's screen for the session's frames."
  (let ((terminal (localgroup-screen-terminal screen)))
    (setf (localgroup-screen-token screen) (terminal-screen-enter terminal)
          (localgroup-screen-active-p screen) t)
    (terminal-screen-invalidate terminal (localgroup-screen-painter screen)))
  nil)

(-> localgroup-screen--leave (localgroup-screen) null)
(defun localgroup-screen--leave (screen)
  "Give the client's screen back and forget the session's frame."
  (when (localgroup-screen-active-p screen)
    (let ((terminal (localgroup-screen-terminal screen)))
      (setf (localgroup-screen-active-p screen) nil
            (localgroup-screen-frame screen) nil)
      (unwind-protect
           (terminal-screen-leave terminal (shiftf (localgroup-screen-token screen) nil))
        (terminal-screen-invalidate terminal (localgroup-screen-painter screen)))))
  nil)

(-> localgroup-screen--paint (localgroup-screen) null)
(defun localgroup-screen--paint (screen)
  "Paint the session's frame, beside the side panel when it shows, on the client's terminal."
  (let ((frame (localgroup-screen-frame screen)))
    (when (and frame (localgroup-screen-active-p screen))
      (destructuring-bind (cursor-row cursor-column cursor-visible-p)
          (localgroup-screen-cursor screen)
        (let ((terminal (localgroup-screen-terminal screen))
              (painter  (localgroup-screen-painter screen)))
          (ecase (localgroup-screen-layout screen)
            (:session
             (terminal-screen-paint terminal painter (coerce frame 'list)
                                    :cursor-row       cursor-row
                                    :cursor-column    cursor-column
                                    :cursor-visible-p cursor-visible-p))
            (:beside
             (terminal-screen-paint terminal painter (localgroup-screen--composed-rows screen frame)
                                    :cursor-row       cursor-row
                                    :cursor-column    (+ cursor-column 1 (localgroup-screen--panel-width screen))
                                    :cursor-visible-p cursor-visible-p))
            (:alone
             (terminal-screen-paint terminal painter
                                    (screen-panel-rows (localgroup-screen-panel screen) terminal
                                                       :height (max 1 (terminal-rows terminal))
                                                       :width  (terminal-columns terminal))
                                    :cursor-row       0
                                    :cursor-column    0
                                    :cursor-visible-p nil)))))))
  nil)

(-> localgroup-screen--composed-rows (localgroup-screen vector) list)
(defun localgroup-screen--composed-rows (screen frame)
  "Return FRAME's rows with the side panel's rows before them, separated by a rule."
  (let* ((terminal   (localgroup-screen-terminal screen))
         (height     (max 1 (terminal-rows terminal)))
         (panel-rows (screen-panel-rows (localgroup-screen-panel screen) terminal
                                        :height height
                                        :width  (localgroup-screen--panel-width screen)))
         (rule       (terminal--render-spans terminal (list (terminal-span ':dim "│")))))
    (loop for index below height
          collect (concatenate 'string
                               (or (nth index panel-rows) "")
                               rule
                               (if (< index (length frame))
                                   (aref frame index)
                                   "")))))
