(in-package #:autolith)

;;;; -- Application transport adapter --

(-> terminal-file-descriptor-size (integer) (values (option integer) (option integer)))
(defun terminal-file-descriptor-size (file-descriptor)
  "Read native terminal dimensions through Clinedi's POSIX adapter."
  (clinedi:terminal-file-descriptor-size file-descriptor))

(defmethod terminal--write ((terminal stream-terminal) (text string))
  "Write trusted application presentation to the stream transport."
  (write-string text (stream-terminal-output-stream terminal))
  nil)

(defmethod clinedi:terminal-write ((terminal stream-terminal) text)
  "Route native protocol output through the application write extension point."
  (terminal--write terminal text))


;;;; -- Public Construction --

(-> stream-terminal-create
    (&key
     (:input-stream stream)
     (:output-stream stream)
     (:input-file-descriptor integer)
     (:rows integer)
     (:columns integer))
    stream-terminal)
(defun stream-terminal-create
    (&key
       (input-stream *standard-input*)
       (output-stream *standard-output*)
       (input-file-descriptor 0)
       (rows *terminal-default-rows*)
       (columns *terminal-default-columns*))
  "Create a stream terminal using INPUT-STREAM, OUTPUT-STREAM, and a POSIX descriptor."
  (make-instance 'stream-terminal
                 :input-stream input-stream
                 :output-stream output-stream
                 :input-file-descriptor input-file-descriptor
                 :rows (if (plusp rows)
                           rows
                           *terminal-default-rows*)
                 :columns (if (plusp columns)
                              columns
                              *terminal-default-columns*)))


(-> terminal--terminal-mode-or-nil (stream-terminal) t)
(defun terminal--terminal-mode-or-nil (terminal)
  "Capture native mode through the transport adapter for attachment integration."
  (clinedi:terminal-capture-input-mode terminal))
