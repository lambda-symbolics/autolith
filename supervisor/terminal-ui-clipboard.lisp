(in-package #:autolith-supervisor)

;;;; -- Terminal Clipboard (OSC 52) --

(serapeum/types:-> terminal-ui-clipboard-writer (terminal-ui) function)
(defun terminal-ui-clipboard-writer (ui)
  "Return the writer through which sophisticated-clipboard reaches UI's terminal.

The writer sends one OSC 52 control under the UI lock and returns whether it
was sent. Only an interactive styled terminal receives the control; piped or
plain output never carries clipboard requests."
  (lambda (control)
    (let ((terminal (terminal-ui-terminal ui)))
      (if (and (clinedi:terminal-interactive-p terminal)
               (clinedi:terminal-styled-p terminal))
          (with-terminal-ui-locked (ui)
            (terminal--write terminal control)
            (clinedi:terminal-flush terminal)
            t)
          nil))))
