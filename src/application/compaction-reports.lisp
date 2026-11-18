(in-package #:autolith)

;;;; -- Compaction Report Presentation --

(-> application-present-compaction-report (application list) null)
(defun application-present-compaction-report (application details)
  "Announce a saved report to terminal, plain and ACP clients, with an optional copy action."
  (let ((path (getf details :path)))
    (application-present
     application
     (append
      (if path
          (application-notice-line (format nil "compaction bug report saved to ~A" path))
          (application-failure-line
           (format nil "could not save the compaction bug report: ~A" (getf details :save-error))))
      (when (and path (application-ui application))
        (handler-case
            (list (terminal-span ':plain (format nil "~%  "))
                  (termdown:make-widget
                   ':code-copy "(copy bug report to clipboard)"
                   (list ':copy (uiop:read-file-string path :external-format ':utf-8))))
          (error () nil))))))
  nil)
