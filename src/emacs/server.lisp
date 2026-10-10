(in-package #:autolith)

;;;; -- Emacs Server Client --

;;; The person's Emacs runs a server (server-start) on a Unix socket; its path
;;; arrives as :EMACS-SERVER-SOCKET, which autolith.el sets. Requests use the
;;; same line protocol emacsclient speaks: one line of &-quoted arguments, here
;;; a single -eval, answered by -print and -print-nonl chunks or an -error,
;;; after which Emacs closes the connection.
;;;
;;; Every argument travels base64 encoded and every value comes back as base64
;;; UTF-8 JSON, so no Lisp text is ever escaped for the elisp reader and the
;;; request line stays ASCII whatever the server's coding systems are.

(defparameter *emacs-server-timeout* 10
  "Seconds one Emacs server request may wait for its reply.")

(defparameter *emacs-server-maximum-reply* (* 4 1024 1024)
  "The largest reply, in characters, accepted from the Emacs server.")


;;;; -- Availability --

(-> emacs-server-available-p ((option configuration)) boolean)
(defun emacs-server-available-p (configuration)
  "Return true when CONFIGURATION names an Emacs server socket that exists here."
  (let ((socket (and configuration (config :emacs-server-socket configuration))))
    (and socket
         (platform-supports-p *platform* ':local-sockets)
         (probe-file socket)
         t)))


;;;; -- Requests --

(-> emacs-server-call (configuration string &rest string) json-object)
(defun emacs-server-call (configuration body &rest arguments)
  "Evaluate elisp BODY in the configured Emacs and return its value as a JSON object.

BODY sees ARGUMENTS, all strings, as the variables A0, A1 and so on, and must
return a value json-serialize accepts: a plist, vector, string, number, t,
:false or :null."
  (let* ((socket (or (config :emacs-server-socket configuration)
                     (error 'emacs-server-error
                            :message "No Emacs server socket is configured."
                            :reason ':unavailable)))
         (reply (emacs-server-evaluate socket (emacs-server--wrap body arguments))))
    (emacs-server--decode-reply reply)))

(-> emacs-server-evaluate (pathname string &key (:timeout (real 0))) string)
(defun emacs-server-evaluate (socket form &key (timeout *emacs-server-timeout*))
  "Send elisp FORM to the Emacs server at SOCKET and return the printed value text."
  (let ((connection (handler-case (platform-connect-local *platform* socket)
                      (error (condition)
                        (error 'emacs-server-error
                               :message (format nil "The Emacs server at ~A is not reachable: ~A"
                                                (uiop:native-namestring socket) condition)
                               :reason ':unavailable)))))
    (unwind-protect
         (let ((stream (sb-bsd-sockets:socket-make-stream
                        connection :input t :output t :element-type 'character
                                   :external-format ':utf-8 :buffering ':full
                                   :timeout timeout)))
           (handler-case
               (progn
                 (write-string "-eval " stream)
                 (write-string (emacs-server-quote form) stream)
                 (write-char #\Newline stream)
                 (finish-output stream)
                 (emacs-server--read-reply stream))
             (sb-sys:io-timeout ()
               (error 'emacs-server-error
                      :message (format nil "Emacs did not answer within ~A seconds; it may be waiting in the minibuffer."
                                       timeout)
                      :reason ':timeout))
             (stream-error (condition)
               (error 'emacs-server-error
                      :message (format nil "The Emacs server connection failed: ~A" condition)
                      :reason ':protocol))))
      (ignore-errors (sb-bsd-sockets:socket-close connection)))))


;;;; -- Quoting --

(-> emacs-server-quote (string) string)
(defun emacs-server-quote (text)
  "Quote TEXT the way server.el's server-quote-arg does, so it contains no space."
  (with-output-to-string (stream)
    (loop for character across text
          do (case character
               (#\& (write-string "&&" stream))
               (#\- (write-string "&-" stream))
               (#\Newline (write-string "&n" stream))
               (#\Space (write-string "&_" stream))
               (t (write-char character stream))))))

(-> emacs-server-unquote (string) string)
(defun emacs-server-unquote (text)
  "Undo server.el's &-quoting of TEXT."
  (with-output-to-string (stream)
    (loop with index = 0
          while (< index (length text))
          do (let ((character (char text index)))
               (if (and (char= character #\&) (< (1+ index) (length text)))
                   (progn
                     (write-char (case (char text (1+ index))
                                   (#\& #\&)
                                   (#\- #\-)
                                   (#\n #\Newline)
                                   (t #\Space))
                                 stream)
                     (incf index 2))
                   (progn
                     (write-char character stream)
                     (incf index)))))))


;;;; -- Private --

(-> emacs-server--wrap (string list) string)
(defun emacs-server--wrap (body arguments)
  "Return the elisp form binding ARGUMENTS around BODY and encoding its value."
  (format nil "(base64-encode-string (encode-coding-string (json-serialize (let (~{~A~^ ~}) ~A)) 'utf-8) t)"
          (loop for argument in arguments
                for index from 0
                collect (format nil "(a~D (decode-coding-string (base64-decode-string ~S) 'utf-8))"
                                index
                                (usb8-array-to-base64-string (utf8-string-to-octets argument))))
          body))

(-> emacs-server--read-reply (stream) string)
(defun emacs-server--read-reply (stream)
  "Read reply lines from STREAM until Emacs closes it and return the joined -print text.

The text is the printed value without the newline Emacs appends to it."
  (let ((value (make-string-output-stream))
        (size 0))
    (loop for line = (read-line stream nil nil)
          while line
          do (incf size (length line))
             (when (> size *emacs-server-maximum-reply*)
               (error 'emacs-server-error
                      :message "The Emacs reply is larger than the client accepts."
                      :reason ':protocol))
             (cond
               ((uiop:string-prefix-p "-print-nonl " line)
                (write-string (emacs-server-unquote (subseq line 12)) value))
               ((uiop:string-prefix-p "-print " line)
                (write-string (emacs-server-unquote (subseq line 7)) value))
               ((uiop:string-prefix-p "-error " line)
                (error 'emacs-server-error
                       :message (format nil "Emacs signaled: ~A"
                                        (string-right-trim '(#\Newline)
                                                           (emacs-server-unquote (subseq line 7))))
                       :reason ':elisp-error))))
    (let ((text (get-output-stream-string value)))
      ;; Emacs ends the printed value with one newline of its own.
      (if (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline))
          (subseq text 0 (1- (length text)))
          text))))

(-> emacs-server--decode-reply (string) json-object)
(defun emacs-server--decode-reply (reply)
  "Decode a printed base64 string REPLY into the JSON object it carries."
  (let ((encoded (string-trim '(#\" #\Space #\Newline) reply)))
    (handler-case
        (let ((value (json-decode (utf8-octets-to-string
                                   (base64-string-to-usb8-array encoded)))))
          (unless (json-object-p value)
            (error 'emacs-server-error
                   :message "Emacs returned JSON that is not an object."
                   :reason ':protocol))
          value)
      (emacs-server-error (condition)
        (error condition))
      (error (condition)
        (error 'emacs-server-error
               :message (format nil "Emacs returned an unreadable value: ~A" condition)
               :reason ':protocol)))))


;;;; -- Conditions --

(define-condition emacs-server-error (autolith-error)
  ((reason
    :initarg :reason
    :reader emacs-server-error-reason
    :type (member :unavailable :timeout :elisp-error :protocol)
    :documentation "Why the request failed."))
  (:documentation "An Emacs server request failed."))
