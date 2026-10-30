;;;; -- POSIX Parent Terminal Host --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-terminal-native-enable-fullscreen () null)
(defun parent-terminal-native-enable-fullscreen ()
  "POSIX terminals need no native output-mode change for ANSI fullscreen."
  nil)

(serapeum/types:-> parent-terminal-native-restore-fullscreen (t) null)
(defun parent-terminal-native-restore-fullscreen (token)
  "Finish the POSIX native output-mode scope."
  (declare (ignore token))
  nil)
