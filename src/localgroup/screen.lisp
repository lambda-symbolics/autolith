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

(defgeneric screen-panel-width (panel)
  (:documentation "Return the columns PANEL takes left of the session's rows."))

(defgeneric screen-panel-minimum-columns (panel)
  (:documentation "Return the narrowest terminal that shows PANEL beside the session."))

(defgeneric screen-panel-refresh (panel)
  (:documentation "Gather PANEL's current content and return whether it changed since the last call."))

(defgeneric screen-panel-rows (panel terminal height)
  (:documentation "Return HEIGHT display rows of PANEL rendered for TERMINAL, each exactly its width."))

(defgeneric screen-panel-shown-p (panel)
  (:documentation "Return whether the person wants PANEL beside the session.

The choice belongs to the panel, which outlives the screens of a terminal's
successive attachments, so hiding it holds across a reconnect or a switch."))

(defgeneric (setf screen-panel-shown-p) (shown-p panel)
  (:documentation "Record whether the person wants PANEL beside the session."))

(defmethod screen-panel-shown-p ((panel t))
  "A panel without a choice of its own is wanted."
  t)

(defmethod (setf screen-panel-shown-p) (shown-p (panel t))
  "A panel without a choice of its own forgets the request."
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
   (panel
    :initarg       :panel
    :initform      nil
    :reader        localgroup-screen-panel
    :documentation "The side panel composed left of the session's rows, or NIL.")
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

(-> localgroup-screen-panel-visible-p (localgroup-screen) boolean)
(defun localgroup-screen-panel-visible-p (screen)
  "Return true when SCREEN's side panel takes part of the terminal."
  (let ((panel (localgroup-screen-panel screen)))
    (and panel
         (screen-panel-shown-p panel)
         (>= (terminal-columns (localgroup-screen-terminal screen))
             (screen-panel-minimum-columns panel))
         t)))

(-> localgroup-screen-pane-size (localgroup-screen) (values integer integer))
(defun localgroup-screen-pane-size (screen)
  "Return the rows and columns the session's pane gets of SCREEN's terminal."
  (let ((terminal (localgroup-screen-terminal screen)))
    (values (terminal-rows terminal)
            (if (localgroup-screen-panel-visible-p screen)
                (max 1 (- (terminal-columns terminal)
                          (screen-panel-width (localgroup-screen-panel screen))
                          1))
                (terminal-columns terminal)))))

(-> localgroup-screen-take-resize (localgroup-screen) boolean)
(defun localgroup-screen-take-resize (screen)
  "Return whether the pane changed size since the last call, clearing the note."
  (with-lock-held ((localgroup-screen-lock screen))
    (shiftf (localgroup-screen-resize-pending-p screen) nil)))

(-> localgroup-screen-filter-event (localgroup-screen t) t)
(defun localgroup-screen-filter-event (screen event)
  "Return EVENT as the session should see it, or :IGNORE when SCREEN consumed it.

The toggle key shows or hides the side panel. A click lands in the panel,
where it does nothing, or in the session's pane, whose columns start after
the panel."
  (cond
    ((equal event *localgroup-screen-toggle-event*)
     (localgroup-screen-toggle-panel screen)
     ':ignore)
    ((and (typep event '(cons (eql :click) (cons integer (cons integer null))))
          (localgroup-screen-panel-visible-p screen))
     (let ((offset (1+ (screen-panel-width (localgroup-screen-panel screen)))))
       (if (<= (second event) offset)
           ':ignore
           (list ':click (- (second event) offset) (third event)))))
    (t
     event)))

(-> localgroup-screen-toggle-panel (localgroup-screen) null)
(defun localgroup-screen-toggle-panel (screen)
  "Show or hide SCREEN's side panel, noting the pane's new size for the session.

The choice is recorded on the panel, so the next screen of this terminal
starts the way the person left it."
  (let ((panel (localgroup-screen-panel screen)))
    (when panel
      (with-lock-held ((localgroup-screen-lock screen))
        (let ((visible-p (localgroup-screen-panel-visible-p screen)))
          (setf (screen-panel-shown-p panel) (not (screen-panel-shown-p panel)))
          (unless (eq visible-p (localgroup-screen-panel-visible-p screen))
            (setf (localgroup-screen-resize-pending-p screen) t)
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
      (setf (localgroup-screen-frame screen) frame
            (localgroup-screen-cursor screen)
            (list (getf properties :cursor-row)
                  (getf properties :cursor-column)
                  (not (null (getf properties :cursor-visible-p)))))
      (unless (localgroup-screen-active-p screen)
        (localgroup-screen--enter screen)))
    (not (null frame))))

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
        (let* ((panel-p (localgroup-screen-panel-visible-p screen))
               (offset  (if panel-p
                            (1+ (screen-panel-width (localgroup-screen-panel screen)))
                            0)))
          (terminal-screen-paint (localgroup-screen-terminal screen)
                                 (localgroup-screen-painter screen)
                                 (if panel-p
                                     (localgroup-screen--composed-rows screen frame)
                                     (coerce frame 'list))
                                 :cursor-row       cursor-row
                                 :cursor-column    (+ cursor-column offset)
                                 :cursor-visible-p cursor-visible-p)))))
  nil)

(-> localgroup-screen--composed-rows (localgroup-screen vector) list)
(defun localgroup-screen--composed-rows (screen frame)
  "Return FRAME's rows with the side panel's rows before them, separated by a rule."
  (let* ((terminal   (localgroup-screen-terminal screen))
         (height     (max 1 (terminal-rows terminal)))
         (panel-rows (screen-panel-rows (localgroup-screen-panel screen) terminal height))
         (rule       (terminal--render-spans terminal (list (terminal-span ':dim "│")))))
    (loop for index below height
          collect (concatenate 'string
                               (or (nth index panel-rows) "")
                               rule
                               (if (< index (length frame))
                                   (aref frame index)
                                   "")))))
