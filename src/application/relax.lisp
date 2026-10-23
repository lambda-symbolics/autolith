(in-package #:autolith)

;;;; -- Relax Warning --

(-> application-set-relax (application boolean) null)
(defun application-set-relax (application enabled-p)
  "Set APPLICATION's session-scoped relax warning state."
  (setf (application-relax-p application) enabled-p)
  nil)

(-> application-relax-command (application &optional t) null)
(defun application-relax-command (application &optional (argument ':status))
  "Show or change APPLICATION's session-scoped relax warning."
  (let ((mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil "Relax warning is ~:[disabled~;enabled~]."
                (application-relax-p application))))
      ((eq mode ':on)
       (application-set-relax application t))
      ((eq mode ':off)
       (application-set-relax application nil))
      (t
       (error 'configuration-error
              :message "Usage: (relax t) or (relax nil)."))))
  nil)

(define-application-command application--builtin-relax-command
  (:name "/relax"
   :description "warn against shrinking scope or rushing work"
   :tip "Use `(relax t)` to add a warning to provider prompts, or `(relax nil)` to remove it."
   :busy-behavior :apply
   :terminal-behavior :shared
   :callable t
   :slash-parser application--parse-boolean-control
   :static-options (:on :off))
  (application &optional (mode ':status))
  (application-relax-command application mode)
  ':continue)
