;;;; -- Shared Terminal Relay Adapter --

(in-package #:autolith-supervisor)

(defmethod image-daemon:transport-rows ((terminal terminal))
  "Read the host terminal height."
  (clinedi:terminal-rows terminal))

(defmethod (setf image-daemon:transport-rows) (value (terminal terminal))
  "Set the host terminal height."
  (setf (clinedi:terminal-rows terminal) value))

(defmethod image-daemon:transport-columns ((terminal terminal))
  "Read the host terminal width."
  (clinedi:terminal-columns terminal))

(defmethod (setf image-daemon:transport-columns) (value (terminal terminal))
  "Set the host terminal width."
  (setf (clinedi:terminal-columns terminal) value))

(defmethod image-daemon:transport-started-p ((terminal terminal))
  "Read whether the terminal has started."
  (clinedi:terminal-started-p terminal))

(defmethod (setf image-daemon:transport-started-p) (value (terminal terminal))
  "Record whether the terminal has started."
  (setf (clinedi:terminal-started-p terminal) value))

(defmethod image-daemon:transport-interactive-p ((terminal terminal))
  "Read whether the terminal accepts input."
  (clinedi:terminal-interactive-p terminal))

(defmethod (setf image-daemon:transport-interactive-p) (value (terminal terminal))
  "Record whether the terminal accepts input."
  (setf (clinedi:terminal-interactive-p terminal) value))

(defmethod image-daemon:transport-styled-p ((terminal terminal))
  "Read whether the terminal supports styling."
  (clinedi:terminal-styled-p terminal))

(defmethod (setf image-daemon:transport-styled-p) (value (terminal terminal))
  "Record whether the terminal supports styling."
  (setf (clinedi:terminal-styled-p terminal) value))

(defmethod image-daemon:transport-start ((terminal stream-terminal))
  "Start the direct host terminal."
  (clinedi:terminal-start terminal))

(defmethod image-daemon:transport-stop ((terminal stream-terminal))
  "Stop the direct host terminal."
  (clinedi:terminal-stop terminal))

(defmethod image-daemon:transport-write ((terminal stream-terminal) text)
  "Write parent-rendered text to the direct host terminal."
  (clinedi:terminal-write terminal text))

(defmethod image-daemon:transport-flush ((terminal stream-terminal))
  "Flush the direct host terminal."
  (clinedi:terminal-flush terminal))

(defmethod image-daemon:transport-input-ready-p ((terminal stream-terminal))
  "Check direct host input readiness."
  (clinedi:terminal-input-ready-p terminal))

(defmethod image-daemon:transport-read-event ((terminal stream-terminal))
  "Read one direct host editor event."
  (clinedi:terminal-read-event terminal))

(defmethod image-daemon:transport-set-dimensions
    ((terminal terminal) columns &key rows)
  "Resize TERMINAL to COLUMNS and, when supplied, ROWS."
  (clinedi:terminal-set-dimensions terminal columns :rows rows))
