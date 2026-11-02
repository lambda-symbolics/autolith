(in-package #:autolith)

;;;; -- Product Terminal Adapter --

(defclass localgroup-terminal (terminal image-daemon:relay) ()
  (:documentation "An image-daemon relay integrated with Autolith terminal state."))

(-> localgroup-terminal-create (&optional (option terminal)) localgroup-terminal)
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
