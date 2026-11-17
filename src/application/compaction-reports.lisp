(in-package #:autolith)

;;;; -- Compaction Report Presentation --

(-> application-present-compaction-report (application list) null)
(defun application-present-compaction-report (application details)
  "Announce a saved report to terminal, plain and ACP clients, with an optional copy action."
  (let ((path (getf details :path)))
    (application-present
     application
     (append
      (list (terminal-span
             ':notice
             (if path
                 (format nil "Compaction bug report saved to ~A" path)
                 (format nil "Could not save the compaction bug report: ~A" (getf details :save-error)))))
      (when (and path (application-ui application))
        (handler-case
            (list (terminal-span ':plain (format nil "~%  "))
                  (termdown:make-widget
                   ':code-copy "(copy bug report to clipboard)"
                   (list ':copy (uiop:read-file-string path :external-format ':utf-8))))
          (error () nil))))))
  nil)
