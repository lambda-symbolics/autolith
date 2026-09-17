(in-package #:autolith)

(defun lsp-transport-tests--frame (object)
  "Return OBJECT encoded as a native LSP frame."
  (let ((body (json-encode-utf8 object)))
    (concatenate '(vector (unsigned-byte 8))
                 (sb-ext:string-to-octets
                  (format nil "Content-Length: ~D~C~C~C~C" (length body)
                          #\Return #\Newline #\Return #\Newline))
                 body)))

(defun test-lsp-transport-framing ()
  "Test UTF-8 framing, consecutive frames, and malformed input rejection."
  (let* ((object (json-object "jsonrpc" "2.0" "method" "žluť"))
         (bytes (lsp-transport-tests--frame object))
         (stream (flexi-streams:make-in-memory-input-stream
                  (concatenate '(vector (unsigned-byte 8)) bytes bytes))))
    (dotimes (index 2)
      (test-assert (equal (json-get (lsp-read-message stream) "method") "žluť")
                   "each frame is decoded by UTF-8 octet length"))
    (test-assert (null (lsp-read-message stream)) "clean EOF is distinct from malformed input"))
  (dolist (text (list (format nil "Content-Length: nope~C~C~C~C{}" #\Return #\Newline #\Return #\Newline)
                     (format nil "Content-Length: 2~C~CContent-Length: 2~C~C~C~C{}"
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                     "Content-Length: 10"
                     (format nil "Content-Length: 4~C~C~C~C{}" #\Return #\Newline #\Return #\Newline)))
    (test-assert (handler-case
                    (progn (lsp-read-message (flexi-streams:make-in-memory-input-stream
                                               (sb-ext:string-to-octets text))) nil)
                  (lsp-error () t))
                 "malformed headers and truncated frames signal protocol failure")))

(defun test-lsp-transport-write-frame ()
  "Round-trip a generated frame with multibyte Unicode content."
  (let ((stream (make-in-memory-output-stream)))
    (lsp-write-message stream (json-object "text" "ž😀"))
    (let* ((bytes (get-output-stream-sequence stream))
           (decoded (lsp-read-message (flexi-streams:make-in-memory-input-stream bytes))))
      (test-assert (string= (json-get decoded "text") "ž😀")
                   "writer and reader preserve Unicode and CRLF framing"))))


(-> lsp-transport-tests--server-form () list)
(defun lsp-transport-tests--server-form ()
  "Return a standalone ASCII stdio fixture with a callback, timeout, and EOF."
  '(labels ((read-frame ()
              (let ((size nil))
                (loop for line = (read-line *standard-input* nil nil)
                      while (and line (plusp (length (string-trim '(#\Return) line))))
                      do (when (search "Content-Length:" line)
                           (setf size (parse-integer line :start 15 :junk-allowed t))))
                (when size
                  (let ((body (make-string size)))
                    (read-sequence body *standard-input*)
                    body))))

            (write-frame (body)
              (format t "Content-Length: ~D~C~C~C~C~A"
                      (length body) #\Return #\Newline #\Return #\Newline body)
              (finish-output)))
     (read-frame)
     (write-frame "{\"jsonrpc\":\"2.0\",\"id\":\"callback\",\"method\":\"fixture\",\"params\":null}")
     (unless (search "ack" (read-frame)) (sb-ext:exit :code 2))
     (write-frame "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":true}")
     (read-frame)
     (read-frame)
     (read-frame)
     (sb-ext:exit :code 0)))

(-> lsp-transport-tests--open (pathname list) lsp-transport)
(defun lsp-transport-tests--open (root form)
  "Start an isolated SBCL fixture over the actual stdio transport."
  (lsp-transport-open
   :command (namestring sb-ext:*runtime-pathname*)
   :arguments (list "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive"
                    "--eval" (with-standard-io-syntax
                               (let ((*package* (find-package '#:autolith)))
                                 (prin1-to-string form))))
   :directory root
   :request-handler (lambda (method params)
                      (declare (ignore params))
                      (assert (string= method "fixture"))
                      "ack")
   :notification-handler (lambda (method params) (declare (ignore method params)))))

(-> test-lsp-transport-process-lifecycle () null)
(defun test-lsp-transport-process-lifecycle ()
  "Exercise real pipes, callback replies, request deadlines, EOF, and process cleanup."
  (with-test-configuration (configuration)
    (let* ((transport (lsp-transport-tests--open
                       (configuration-working-directory configuration)
                       (lsp-transport-tests--server-form)))
           (process (lsp-transport-process transport))
           (threads (copy-list (lsp-transport-threads transport))))
      (unwind-protect
           (progn
             (test-assert (eq t (lsp-transport-request transport "roundtrip" nil :timeout 10))
                          "real server answers after a client callback")
             (test-assert (handler-case
                              (progn (lsp-transport-request transport "timeout" nil :timeout 0.1) nil)
                            (lsp-timeout () t))
                          "unanswered request times out")
             (test-assert (zerop (hash-table-count (lsp-transport-pending transport)))
                          "timeout removes the pending request")
             (test-assert (handler-case
                              (progn (lsp-transport-request transport "eof" nil :timeout 10) nil)
                            (lsp-timeout () nil)
                            (lsp-error () t))
                          "server EOF wakes a waiting request without waiting for its deadline"))
        (lsp-transport-close transport))
      (test-assert (not (uiop:process-alive-p process)) "closed server is reaped")
      (test-assert (every (lambda (thread) (not (thread-alive-p thread))) threads)
                   "all transport threads have stopped"))
    (let* ((transport (lsp-transport-tests--open
                       (configuration-working-directory configuration) '(sleep 60)))
           (process (lsp-transport-process transport)))
      (unwind-protect (lsp-transport-close transport)
        (when (lsp-transport-process transport) (lsp-transport-close transport)))
      (test-assert (not (uiop:process-alive-p process))
                   "close terminates a server that ignores closed stdin")))
  nil)
