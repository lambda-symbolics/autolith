(in-package #:autolith-supervisor)

;;;; -- Application transport adapter --

(serapeum/types:-> terminal-current-size () (values integer integer))
(defun terminal-current-size ()
  "Return current terminal rows and columns through Clinedi's size lookup."
  (clinedi:terminal-current-size
   :file-descriptor (clinedi:terminal-standard-input-file-descriptor)
   :default-rows    clinedi:*terminal-default-rows*
   :default-columns clinedi:*terminal-default-columns*))

(defmethod terminal--write ((terminal stream-terminal) (text string))
  "Write trusted application presentation to the stream transport."
  (write-string text (clinedi:stream-terminal-output-stream terminal))
  nil)

(defmethod clinedi:terminal-write ((terminal stream-terminal) text)
  "Route native protocol output through the application write extension point."
  (terminal--write terminal text))


;;;; -- Public Construction --

(serapeum/types:-> stream-terminal-create
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
       (rows clinedi:*terminal-default-rows*)
       (columns clinedi:*terminal-default-columns*))
  "Create a stream terminal using INPUT-STREAM, OUTPUT-STREAM, and a POSIX descriptor."
  (make-instance 'stream-terminal
                 :input-stream          input-stream
                 :output-stream         output-stream
                 :input-file-descriptor input-file-descriptor
                 :rows                  (if (plusp rows)
                                            rows
                                            clinedi:*terminal-default-rows*)
                 :columns               (if (plusp columns)
                                            columns
                                            clinedi:*terminal-default-columns*)))

(serapeum/types:-> terminal--terminal-mode-or-nil (stream-terminal) t)
(defun terminal--terminal-mode-or-nil (terminal)
  "Capture native mode through the transport adapter for attachment integration."
  (clinedi:terminal-capture-input-mode terminal))
