(in-package #:autolith)

;;;; -- Application Paste Policy --

(defparameter *terminal-control-v-paste-idle-seconds* 0.05
  "The idle gap ending one unbracketed Ctrl-V paste burst.")

(-> terminal--read-control-v-paste (stream) t)
(defun terminal--read-control-v-paste (stream)
  "Read literal Ctrl-V paste using the application's idle gap and input bound."
  (clinedi:read-paste-burst stream
                          :idle-seconds *terminal-control-v-paste-idle-seconds*
                          :maximum-characters *terminal-unbracketed-paste-maximum-characters*))

(-> terminal--decode-editing-event
    (stream &key (:escape-delay real))
    t)
(defun terminal--decode-editing-event
    (stream &key (escape-delay *terminal-escape-delay-seconds*))
  "Decode one Clinedi event while retaining Autolith's literal Ctrl-V policy."
  (let ((character (read-char stream nil nil)))
    (cond
      ((null character)
       ':stream-end)
      ((char= character (code-char 22))
       (terminal--read-control-v-paste stream))
      (t
       (unread-char character stream)
       (read-event :stream stream :escape-delay escape-delay)))))
