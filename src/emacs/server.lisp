(in-package #:autolith)

;;;; -- Emacs Server Client --

(defparameter *emacs-server-timeout* 10
  "Seconds one Emacs server request may wait for its reply.")

(defparameter *emacs-server-maximum-reply* (* 8 1024 1024)
  "The largest reply, in characters, accepted from the Emacs server.

Large enough for an emacs: buffer of *EMACS-BUFFER-MAXIMUM-CHARACTERS*
characters, encoded as base64 UTF-8 JSON.")


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

Bind ARGUMENTS, which must be strings, to A0, A1 and so on. BODY must return
a value accepted by json-serialize: a plist, vector, string, number, t, :false
or :null."
  (let* ((socket (or (config :emacs-server-socket configuration)
                     (error 'emacs-server-error
                            :message "No Emacs server socket is configured."
                            :reason  ':unavailable)))
         (reply  (emacs-server-evaluate socket (emacs-server--wrap body arguments))))
    (emacs-server--decode-reply reply)))

(-> emacs-server-evaluate (pathname string &key (:timeout (real 0))) string)
(defun emacs-server-evaluate (socket form &key (timeout *emacs-server-timeout*))
  "Evaluate FORM through the standalone bounded socket client."
  (handler-case
      (lisp-em:evaluate socket form :timeout       timeout
                                    :maximum-reply *emacs-server-maximum-reply*)
    (lisp-em:emacs-client-error (condition)
      (error 'emacs-server-error
             :message (lisp-em:emacs-client-error-message condition)
             :reason  (lisp-em:emacs-client-error-reason condition)))))


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
                   :reason  ':protocol))
          value)
      (emacs-server-error (condition)
        (error condition))
      (error (condition)
        (error 'emacs-server-error
               :message (format nil "Emacs returned an unreadable value: ~A" condition)
               :reason  ':protocol)))))


;;;; -- Conditions --

(define-condition emacs-server-error (autolith-error)
  ((reason
    :initarg       :reason
    :reader        emacs-server-error-reason
    :type          (member :unavailable :timeout :elisp-error :protocol)
    :documentation "Why the request failed."))
  (:documentation "An Emacs server request failed."))
