(in-package #:autolith)

;;;; -- Papercut Tool Classes --

(defclass papercut-tool (tool)
  ()
  (:documentation "A tool for recording a user-visible report about an Autolith problem."))

(defclass papercut-report-tool (papercut-tool)
  ()
  (:documentation "Record one new papercut report."))


;;;; -- Tool Results --

(defparameter *papercut-tool-duplicate-note*
  "already-reported: this problem is already active as the report above; nothing new was recorded, so do not report it again."
  "The acknowledgement line telling the model its report repeated an active one.")

(-> papercut-tool--result (papercut boolean) string)
(defun papercut-tool--result (papercut duplicate-p)
  "Return the bounded acknowledgement, noting whether PAPERCUT was already active."
  (format nil
          "papercut-id: ~A~%title: ~A~@[~%~A~]"
          (papercut-identifier papercut)
          (papercut-title papercut)
          (and duplicate-p *papercut-tool-duplicate-note*)))


;;;; -- Tool Executions --

(defmethod tool-child-safe-p ((tool papercut-report-tool))
  "Allow explicit reporting grants in task roles and noninteractive jobs."
  t)

(defmethod tool-conversation-persistence ((tool papercut-report-tool))
  "Keep papercut calls and their acknowledgements across responses and compaction."
  (declare (ignore tool))
  ':durable)

(defmethod tool-compact-result-visible-p ((tool papercut-report-tool))
  "Keep every successful papercut report visible in compact presentation."
  t)

(defmethod tool-execute ((tool papercut-report-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Record a papercut using the full title and content provided.

If the report duplicates an active papercut in this workspace, return the
existing report instead."
  (declare (ignore tool))
  (let ((title   (tool-argument arguments "title" :required t))
        (content (tool-argument arguments "content" :required t)))
    (unless (and (stringp title) (stringp content))
      (error 'tool-error
             :message   "papercut.report requires string values for title and content."
             :tool-name "papercut.report"))
    (multiple-value-bind (papercut duplicate-p)
        (papercut-report
         (tool-context-configuration context)
         :title      title
         :content    content
         :diagnostic (tool-argument arguments "diagnostic")
         :issue-kind (or (tool-argument arguments "issue-kind") "other")
         :tool       (tool-argument arguments "tool")
         :source-conversation
         (conversation-identifier (tool-context-conversation context)))
      (tool-success (papercut-tool--result papercut duplicate-p)))))
