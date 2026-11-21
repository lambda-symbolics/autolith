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
  "Evaluate elisp BODY in the configured Emacs and return its JSON object.

Bind string ARGUMENTS to A0, A1 and so on. BODY must return an object
accepted by json-serialize."
  (let* ((socket (or (config :emacs-server-socket configuration)
                     (error 'emacs-server-error
                            :message "No Emacs server socket is configured."
                            :reason  ':unavailable)))
         (value  (handler-case
                     (lisp-em:evaluate-json socket body
                                            :arguments     arguments
                                            :timeout       *emacs-server-timeout*
                                            :maximum-reply *emacs-server-maximum-reply*)
                   (lisp-em:emacs-client-error (condition)
                     (error 'emacs-server-error
                            :message (lisp-em:emacs-client-error-message condition)
                            :reason  (lisp-em:emacs-client-error-reason condition))))))
    (unless (json-object-p value)
      (error 'emacs-server-error
             :message "Emacs returned JSON that is not an object."
             :reason  ':protocol))
    value))

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


;;;; -- Conditions --

(define-condition emacs-server-error (autolith-error)
  ((reason
    :initarg       :reason
    :reader        emacs-server-error-reason
    :type          (member :unavailable :timeout :elisp-error :protocol)
    :documentation "Why the request failed."))
  (:documentation "An Emacs server request failed."))
