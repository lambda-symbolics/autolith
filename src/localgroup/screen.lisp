(in-package #:autolith)

;;;; -- Client Screen --

;;; A terminal client of a hosted fullscreen session receives the session's
;;; screen as frames of display rows rather than as painted output, and
;;; paints them on its own terminal. The client therefore owns the alternate
;;; buffer, the frame painter and the cursor, and can compose its own rows
;;; around the session's.

(defparameter *localgroup-screen-dimension-limit* 10000
  "The largest frame height or width a client accepts from a session.")

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
    :documentation "The cursor row, column and visibility of the frame."))
  (:documentation "The whole screen a client paints from a hosted session's frames."))

(-> localgroup-screen-create (&key (:terminal terminal)) localgroup-screen)
(defun localgroup-screen-create (&key terminal)
  "Return a screen painting a session's frames on the client's TERMINAL."
  (make-instance 'localgroup-screen :terminal terminal))

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
  "Paint the session's frame on the client's terminal."
  (let ((frame (localgroup-screen-frame screen)))
    (when (and frame (localgroup-screen-active-p screen))
      (destructuring-bind (cursor-row cursor-column cursor-visible-p)
          (localgroup-screen-cursor screen)
        (terminal-screen-paint (localgroup-screen-terminal screen)
                               (localgroup-screen-painter screen)
                               (coerce frame 'list)
                               :cursor-row       cursor-row
                               :cursor-column    cursor-column
                               :cursor-visible-p cursor-visible-p))))
  nil)
