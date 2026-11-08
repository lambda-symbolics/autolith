(in-package #:autolith)

;;;; -- Usage CLI --

(-> main--usage-command () clingon:command)
(defun main--usage-command ()
  "Return the noninteractive operating-system usage command."
  (make-command
   :name        "usage"
   :description "show stored-data, disk and memory usage"
   :usage       ""
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'configuration-error
              :message "Usage: autolith usage"))
     (handler-case
         (let ((configuration
                 (configuration-create :defer-provider-validation-p t)))
           (usage-print-report (usage-collect configuration)))
       ((or platform-error file-error) (condition)
         (format *error-output* "~A~%" (usage--text condition))
         (uiop:quit 1))))))
