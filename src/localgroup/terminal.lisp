(in-package #:autolith)

;;;; -- Product Terminal Adapter --

(defclass localgroup-terminal (terminal image-daemon:relay) ()
  (:documentation "An image-daemon relay integrated with Autolith terminal state."))

(defun localgroup-terminal-create (&optional direct-terminal)
  "Create a relay terminal initially owned by DIRECT-TERMINAL when supplied."
  (let ((startup-values
         (and *localgroup-startup-record* (rest *localgroup-startup-record*))))
    (make-instance 'localgroup-terminal :direct-terminal direct-terminal :rows
                   (if direct-terminal
                       (terminal-rows direct-terminal)
                       (or (getf startup-values :rows) *terminal-default-rows*))
                   :columns
                   (if direct-terminal
                       (terminal-columns direct-terminal)
                       (or (getf startup-values :columns) *terminal-default-columns*))
                   :interactive-p
                   (and direct-terminal (terminal-interactive-p direct-terminal))
                   :styled-p
                   (if direct-terminal
                       (terminal-styled-p direct-terminal)
                       (not (null (getf startup-values :styled-p)))))))

(defun localgroup-terminal-resize (terminal rows columns styled-p)
  "Adopt client styling and relay client dimensions to the interactive reader.

Dimensions are deliberately not applied here: writing them directly
desynchronizes the composed row width from the live-region geometry, and
every following repaint then eats one scrollback line while the live
region climbs toward the top of the screen. TERMINAL-UI-RESIZE is the
only dimension writer that keeps both in step and repaints."
  (with-lock-held ((image-daemon:relay-lock terminal))
    (setf (terminal-interactive-p terminal) t
          (terminal-styled-p terminal) (not (null styled-p))))
  (terminal-relayed-resize-publish rows columns)
  nil)

(defmethod image-daemon:transport-rows ((terminal terminal)) (terminal-rows terminal))

(defmethod (setf image-daemon:transport-rows) (value (terminal terminal))
  (setf (terminal-rows terminal) value))

(defmethod image-daemon:transport-columns ((terminal terminal))
  (terminal-columns terminal))

(defmethod (setf image-daemon:transport-columns) (value (terminal terminal))
  (setf (terminal-columns terminal) value))

(defmethod image-daemon:transport-started-p ((terminal terminal))
  (terminal-started-p terminal))

(defmethod (setf image-daemon:transport-started-p) (value (terminal terminal))
  (setf (terminal-started-p terminal) value))

(defmethod image-daemon:transport-interactive-p ((terminal terminal))
  (terminal-interactive-p terminal))

(defmethod (setf image-daemon:transport-interactive-p) (value (terminal terminal))
  (setf (terminal-interactive-p terminal) value))

(defmethod image-daemon:transport-styled-p ((terminal terminal))
  (terminal-styled-p terminal))

(defmethod (setf image-daemon:transport-styled-p) (value (terminal terminal))
  (setf (terminal-styled-p terminal) value))

(defmethod image-daemon:transport-start ((terminal stream-terminal))
  (terminal-start terminal))

(defmethod terminal-start ((terminal localgroup-terminal))
  (image-daemon:transport-start terminal))

(defmethod image-daemon:transport-stop ((terminal stream-terminal))
  (terminal-stop terminal))

(defmethod terminal-stop ((terminal localgroup-terminal))
  (image-daemon:transport-stop terminal))

(defmethod image-daemon:transport-write ((terminal stream-terminal) text)
  (terminal--write terminal text))

(defmethod terminal--write ((terminal localgroup-terminal) text)
  (image-daemon:transport-write terminal text))

(defmethod image-daemon:transport-flush ((terminal stream-terminal))
  (terminal-flush terminal))

(defmethod terminal-flush ((terminal localgroup-terminal))
  (image-daemon:transport-flush terminal))

(defmethod image-daemon:transport-input-ready-p ((terminal stream-terminal))
  (terminal-input-ready-p terminal))

(defmethod terminal-input-ready-p ((terminal localgroup-terminal))
  (image-daemon:transport-input-ready-p terminal))

(defmethod image-daemon:transport-read-event ((terminal stream-terminal))
  (terminal-read-event terminal))

(defmethod terminal-read-event ((terminal localgroup-terminal))
  (image-daemon:transport-read-event terminal))

(defmethod image-daemon:transport-set-dimensions
    ((terminal terminal) columns &key rows)
  (terminal-set-dimensions terminal columns :rows rows))

(defmethod image-daemon:transport-resize
    ((terminal localgroup-terminal) &key rows columns styled-p)
  (setf (terminal-interactive-p terminal) t
        (terminal-styled-p terminal) (not (null styled-p)))
  (terminal-relayed-resize-publish rows columns)
  nil)
