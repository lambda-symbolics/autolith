;;;; -- Windows Parent Terminal Host --

(in-package #:autolith-supervisor)

(sb-alien:define-alien-routine ("GetStdHandle" parent-terminal-native--standard-handle)
    (sb-alien:signed 64)
  (number (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetConsoleMode" parent-terminal-native--console-mode)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (mode (* (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("SetConsoleMode" parent-terminal-native--set-console-mode)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (mode (sb-alien:unsigned 32)))

(serapeum/types:-> parent-terminal-native-enable-fullscreen () (option cons))
(defun parent-terminal-native-enable-fullscreen ()
  "Enable Windows VT output and retain the exact previous console mode."
  (let ((handle (parent-terminal-native--standard-handle #xfffffff5)))
    (sb-alien:with-alien ((mode (sb-alien:unsigned 32)))
      (unless (zerop (parent-terminal-native--console-mode handle (sb-alien:addr mode)))
        (when (zerop (parent-terminal-native--set-console-mode handle
                                                             (logior mode #x0001 #x0004)))
          (error 'parent-terminal-error :reason ':native-output-mode))
        (cons handle mode)))))

(serapeum/types:-> parent-terminal-native-restore-fullscreen (t) null)
(defun parent-terminal-native-restore-fullscreen (token)
  "Restore the console output mode saved before fullscreen entry."
  (when token
    (when (zerop (parent-terminal-native--set-console-mode (first token) (rest token)))
      (error 'parent-terminal-error :reason ':native-output-mode)))
  nil)
