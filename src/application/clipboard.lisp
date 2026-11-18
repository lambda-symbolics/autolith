(in-package #:autolith)

;;;; -- Transcript Actions --

(defparameter *application-transcript-action-notice-seconds* 4
  "How long clipboard and browser notices stay on the status row.")

(defparameter *application-opened-url-notice-cells* 60
  "The longest URL shown whole in an opening notice before it is abbreviated.")

(-> application--transcript-action-notice (application string) null)
(defun application--transcript-action-notice (application text)
  "Show TEXT transiently on APPLICATION's status row."
  (terminal-ui-set-notice (application-ui application)
                          text
                          :duration-seconds
                          *application-transcript-action-notice-seconds*)
  nil)

(-> application-copy-text (application string) boolean)
(defun application-copy-text (application text)
  "Copy TEXT for the user and report how, returning whether any route accepted it.

The host clipboard and the terminal's OSC 52 route both receive TEXT, since
only the terminal reaches the user's own machine over SSH or a localgroup
relay. A notice names the route that worked, or the host failure."
  (let ((lines (1+ (count #\Newline (string-right-trim '(#\Newline) text)))))
    (multiple-value-bind (host-p terminal-p host-condition)
        (clipboard-copy-text
         text
         :terminal-writer (terminal-ui-clipboard-writer (application-ui application)))
      (application--transcript-action-notice
       application
       (cond
         (host-p
          (format nil "copied ~D line~:P to the clipboard" lines))
         (terminal-p
          (format nil "sent ~D line~:P to the terminal's clipboard; ~A" lines host-condition))
         (t
          (format nil "nothing copied: ~A" host-condition))))
      (or host-p terminal-p))))

(-> application-open-url (application string) boolean)
(defun application-open-url (application url)
  "Open web URL in the default browser and report the outcome in a notice."
  (cond
    ((not (web-url-p url))
     (application--transcript-action-notice
      application "only http and https links open from the transcript")
     nil)
    ((platform-open-url *platform* url)
     (application--transcript-action-notice
      application
      (format nil "opening ~A"
              (text-cell-prefix url *application-opened-url-notice-cells*)))
     t)
    (t
     (application--transcript-action-notice
      application "no browser launcher is available on this host")
     nil)))

(-> application-transcript-action (application list) null)
(defun application-transcript-action (application action)
  "Perform clicked transcript ACTION: (:copy TEXT), (:open-url URL) or (:insert TEXT).

Inserting puts TEXT, such as a tool call's Lisp invocation, into the composer."
  (case (first action)
    (:copy
     (application-copy-text application (second action)))
    (:open-url
     (application-open-url application (second action)))
    (:insert
     (terminal-ui-insert-input (application-ui application) (second action))))
  nil)

(-> application-connect-transcript-actions (application) null)
(defun application-connect-transcript-actions (application)
  "Route clicks on APPLICATION's transcript widgets and links to its actions."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (setf (terminal-ui-action-function ui)
            (lambda (action)
              (application-transcript-action application action)))))
  nil)
