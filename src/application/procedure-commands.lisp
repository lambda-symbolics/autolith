(in-package #:autolith)

;;;; -- Procedure Commands --

;;; (procedures) lists the procedures of this workspace. (run-procedure "ID"
;;; :NAME VALUE ...) runs one with ordinary tool authorization.
;;; (retire-procedure) retires one until it is defined again.


;;; (procedures)

(-> application-procedures-entry (application) list)
(defun application-procedures-entry (application)
  "Return terminal spans for the procedures in the workspace of APPLICATION.

Include the parameters and documentation of the procedures."
  (let* ((configuration (application-configuration application))
         (entries       (knowledge-base-procedures (knowledge-base-read configuration)
                                                   (knowledge--workspace configuration))))
    (if (null entries)
        (application-hint-line "no procedures are defined yet")
        (cons (terminal-span ':brand (format nil "# procedures~%"))
              (loop for entry in entries
                    append (application-procedures--entry-lines entry))))))

(-> application-procedures--entry-lines (list) list)
(defun application-procedures--entry-lines (entry)
  "Return the terminal lines describing the procedure ENTRY."
  (handler-case
      (list (if (getf entry :retired-p)
                (terminal-span ':dim "  × ")
                (terminal-span ':success "  ✓ "))
            (terminal-span ':strong
                           (format nil "~A~%" (sanitize-text (procedure-signature entry)
                                                             :single-line-p t)))
            (terminal-span ':dim
                           (format nil "    ~A~%" (sanitize-text (procedure-documentation entry)
                                                                 :single-line-p t))))
    (knowledge-error (condition)
      (list (terminal-span ':failure "  ! ")
            (terminal-span ':strong (sanitize-text (getf entry :id) :single-line-p t))
            (terminal-span ':dim (format nil " ~A~%" (autolith-error-message condition)))))))


;;; (run-procedure), (retire-procedure)

(-> application-procedure-run-entry (application string list) list)
(defun application-procedure-run-entry (application identifier arguments)
  "Run the procedure IDENTIFIER with the keyword ARGUMENTS. Return terminal spans for the result."
  (handler-case
      (list (terminal-span ':success "✓ ran  ")
            (terminal-span ':dim
                           (format nil "~A~%"
                                   (sanitize-text
                                    (procedure-run (application-tool-context application)
                                                   identifier arguments)))))
    (knowledge-error (condition)
      (application-failure-line (autolith-error-message condition)))))

(-> application-procedure-retire-entry (application string) list)
(defun application-procedure-retire-entry (application identifier)
  "Retire the procedure IDENTIFIER. Return terminal spans for the result."
  (handler-case
      (progn
        (procedure-retire (application-configuration application) identifier)
        (list (terminal-span ':success "✓ retired  ")
              (terminal-span ':strong (format nil "~A~%" identifier))))
    (knowledge-error (condition)
      (application-failure-line
       (format nil "~A (procedures) lists them." (autolith-error-message condition))))))


;;;; -- Command Definitions --

(define-application-command application--builtin-procedures-command
    (:name              "procedures"
     :argument          nil
     :description       "list the procedures of this workspace."
     :tip               "shows the procedures and their parameters"
     :busy-behavior     :inspect
     :terminal-behavior :shared
     :callable          t)
    (application)
  (application-present application (application-procedures-entry application))
  ':continue)

(define-application-command application--builtin-run-procedure-command
    (:name              "run-procedure"
     :argument          "[ID] [NAME VALUE ...]"
     :description       "run a procedure without the model."
     :tip               "runs a procedure with keyword arguments"
     :busy-behavior     :hold
     :terminal-behavior :shared
     :callable          t)
    (application &optional identifier &rest arguments)
  (application-present
   application
   (if identifier
       (application-procedure-run-entry application identifier arguments)
       (application-failure-line "usage: (run-procedure \"ID\" :NAME VALUE ...). Run (procedures) to list them.")))
  ':continue)

(define-application-command application--builtin-retire-procedure-command
    (:name              "retire-procedure"
     :argument          "[ID]"
     :description       "retire a procedure until it is defined again."
     :tip               "retires a procedure"
     :busy-behavior     :apply
     :terminal-behavior :shared
     :callable          t)
    (application &optional identifier)
  (application-present
   application
   (if identifier
       (application-procedure-retire-entry application identifier)
       (application-failure-line "usage: (retire-procedure \"ID\"). Run (procedures) to list them.")))
  ':continue)
