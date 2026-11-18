(in-package #:autolith)

;;;; -- Transcript Actions --

(defparameter *application-transcript-action-notice-seconds* 4
  "How long clipboard and browser notices stay on the status row.")

(defparameter *application-opened-url-notice-cells* 60
  "The longest URL shown whole in an opening notice before it is abbreviated.")

(-> terminal-ui--transcript-action-notice (terminal-ui string) null)
(defun terminal-ui--transcript-action-notice (ui text)
  "Show TEXT transiently on UI's status row."
  (terminal-ui-set-notice ui text :duration-seconds *application-transcript-action-notice-seconds*)
  nil)

(-> terminal-ui-copy-text (terminal-ui string) boolean)
(defun terminal-ui-copy-text (ui text)
  "Copy TEXT for UI's user and report how, returning whether any route accepted it.

The host clipboard and the terminal's OSC 52 route both receive TEXT, since
only the terminal reaches the user's own machine over SSH or a localgroup
relay. A notice names the route that worked, or the host failure."
  (let ((lines (1+ (count #\Newline (string-right-trim '(#\Newline) text)))))
    (multiple-value-bind (host-p terminal-p host-condition)
        (clipboard-copy-text text :terminal-writer (terminal-ui-clipboard-writer ui))
      (terminal-ui--transcript-action-notice
       ui
       (cond
         (host-p
          (format nil "copied ~D line~:P to the clipboard" lines))
         (terminal-p
          (format nil "sent ~D line~:P to the terminal's clipboard; ~A" lines host-condition))
         (t
          (format nil "nothing copied: ~A" host-condition))))
      (or host-p terminal-p))))

(-> terminal-ui-open-url (terminal-ui string) boolean)
(defun terminal-ui-open-url (ui url)
  "Open web URL in the default browser of UI's host and report the outcome in a notice."
  (cond
    ((not (web-url-p url))
     (terminal-ui--transcript-action-notice
      ui "only http and https links open from the transcript")
     nil)
    ((platform-open-url *platform* url)
     (terminal-ui--transcript-action-notice
      ui
      (format nil "opening ~A"
              (text-cell-prefix url *application-opened-url-notice-cells*)))
     t)
    (t
     (terminal-ui--transcript-action-notice
      ui "no browser launcher is available on this host")
     nil)))

(-> terminal-ui-transcript-action (terminal-ui list) null)
(defun terminal-ui-transcript-action (ui action)
  "Perform clicked transcript ACTION in UI: (:copy TEXT), (:open-url URL) or (:insert TEXT).

Inserting puts TEXT, such as a tool call's Lisp invocation, into the composer.
All three happen where the terminal is: the clipboard and the browser are the
user's own."
  (when (stringp (second action))
    (case (first action)
      (:copy
       (terminal-ui-copy-text ui (second action)))
      (:open-url
       (terminal-ui-open-url ui (second action)))
      (:insert
       (terminal-ui-insert-input ui (second action)))))
  nil)

(-> terminal-ui-connect-transcript-actions (terminal-ui) null)
(defun terminal-ui-connect-transcript-actions (ui)
  "Route clicks on UI's transcript widgets and links to its actions."
  (setf (terminal-ui-action-function ui)
        (lambda (action)
          (terminal-ui-transcript-action ui action)))
  nil)

(-> application-connect-transcript-actions (application) null)
(defun application-connect-transcript-actions (application)
  "Route clicks on the transcript of APPLICATION's own terminal UI to its actions."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (terminal-ui-connect-transcript-actions ui)))
  nil)
