(in-package #:autolith)

;;;; -- Procedure Commands --

;;; (procedures) lists the procedures Autolith learned in this workspace, what
;;; they call, and what they saved; (run-procedure "ID" :NAME VALUE ...) runs
;;; one with ordinary tool authorization; (retire-procedure) stops one for good.


;;; (procedures)

(-> application-procedures-entry (application) list)
(defun application-procedures-entry (application)
  "Return the learned procedures of APPLICATION's workspace with status, calls, and cost."
  (let* ((configuration (application-configuration application))
         (entries       (knowledge-base-procedures (knowledge-base-read configuration)
                                                   (knowledge--workspace configuration))))
    (if (null entries)
        (application-hint-line
         "no procedures are learned yet; Autolith learns one when three turns make the same tool calls")
        (append
         (list (terminal-span ':brand "# procedures")
               (terminal-span ':dim (format nil "  ~D~%" (length entries))))
         (loop for entry in entries
               append (application-procedures--entry-lines configuration entry))))))

(-> application-procedures--entry-lines (configuration list) list)
(defun application-procedures--entry-lines (configuration entry)
  "Return the terminal lines describing procedure ENTRY."
  (let* ((status     (procedure-entry-status entry))
         (identifier (getf entry :id))
         (version    (procedure-entry-version entry))
         (procedure  (getf version :procedure))
         (baseline   (getf version :baseline))
         (savings    (and (member status '(:active :suspended))
                          (procedure-savings configuration identifier)))
         (request    (procedure-input-text procedure)))
    (append
     (list (ecase status
             (:active
              (terminal-span ':success "  ✓ "))
             (:inactive
              (terminal-span ':notice "  ? "))
             (:suspended
              (terminal-span ':failure "  ! "))
             (:retired
              (terminal-span ':dim "  × ")))
           (terminal-span ':strong
                          (format nil "~A v~D"
                                  (sanitize-text identifier :single-line-p t)
                                  (getf version :version)))
           (terminal-span (ecase status
                            (:active
                             ':success)
                            (:inactive
                             ':notice)
                            (:suspended
                             ':failure)
                            (:retired
                             ':dim))
                          (format nil "  ~(~A~)" status))
           (terminal-span ':dim
                          (format nil "~@[ · runs v~D~] · ~D run~:P, ~D failed~A~%"
                                 (and (getf entry :active)
                                      (/= (getf entry :active) (getf version :version))
                                      (getf entry :active))
                                 (getf entry :runs) (getf entry :failures)
                                 (case status
                                   (:inactive
                                    " · legacy definition")
                                   (:suspended
                                    " · correct its definition to resume")
                                   (otherwise
                                    ""))))
           (terminal-span ':plain
                          (format nil "    ~{~A~^ → ~}~%"
                                  (mapcar (lambda (step)
                                            (sanitize-text (getf step :tool) :single-line-p t))
                                          (getf (rest procedure) :steps)))))
     (when request
       (list (terminal-span ':dim (format nil "    request: ~A~%"
                                          (sanitize-text request :single-line-p t)))))
     (when (eq status ':suspended)
       (list (terminal-span ':failure
                            (format nil "    last failure: ~A~%"
                                    (sanitize-text (or (getf entry :last-failure) "unknown")
                                                   :single-line-p t)))))
     (when (getf version :examples)
       (list (terminal-span ':dim
                            (format nil "    learned from ~D turns averaging ~D request~:P, ~D tokens~@[~A~]~%"
                                    (length (getf version :examples))
                                    (getf baseline :requests) (getf baseline :tokens)
                                    (and savings
                                         (format nil "; turns running it: ~D request~:P, ~D tokens (~D turn~:P)"
                                                 (getf savings :requests) (getf savings :tokens)
                                                 (getf savings :turns))))))))))


;;; (run-procedure), (retire-procedure)

(-> application-procedure-run-entry (application string list) list)
(defun application-procedure-run-entry (application identifier arguments)
  "Run procedure IDENTIFIER with ARGUMENTS, alternating names and values, and show each call."
  (handler-case
      (let* ((parameters (application-procedures--parameters arguments))
             (run        (procedure-run (application-tool-context application) identifier
                                        :parameters parameters :caller ':user))
             (report     (procedure-run-report identifier run)))
        (list (if (eq (getf run :status) ':ok)
                  (terminal-span ':success "✓ ran  ")
                  (terminal-span ':failure "! stopped  "))
              (terminal-span ':dim (format nil "~A~%" (sanitize-text report)))))
    (knowledge-error (condition)
      (application-failure-line (autolith-error-message condition)))))

(-> application-procedures--parameters (list) list)
(defun application-procedures--parameters (arguments)
  "Return the parameter alist of ARGUMENTS, names (keywords or text) alternating with values."
  (unless (evenp (length arguments))
    (knowledge--fail ':invalid-parameters arguments
                     "Give parameters as names and values, such as (run-procedure \"ID\" :quarter \"q3\")."))
  (loop for (name value) on arguments by (partial #'nthcdr 2)
        collect (cons (string-downcase (string name)) value)))

(-> application-procedure-retire-entry (application string) list)
(defun application-procedure-retire-entry (application identifier)
  "Retire procedure IDENTIFIER and confirm what changed."
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
     :description       "list procedures learned from recurring work."
     :tip               "shows all learned tool sequences"
     :busy-behavior     :inspect
     :terminal-behavior :shared
     :callable          t)
    (application)
  (application-present application (application-procedures-entry application))
  ':continue)

(define-application-command application--builtin-run-procedure-command
    (:name              "run-procedure"
     :argument          "[ID] [NAME VALUE ...]"
     :description       "run a learned procedure without the model."
     :tip               "run a saved tool procedure"
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
     :description       "stop a learned procedure for good"
     :tip               "retire a procedure so it is no longer used"
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
