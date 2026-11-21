(in-package #:autolith)

;;;; -- Local Task Assessment --

(-> application-refinement-command (application &optional t t &rest t) null)
(defun application-refinement-command (application &optional action identifier &rest options)
  "List proposals or supply a native local task assessment through the reviewer."
  (unless (or (and (null action) (null identifier) (null options))
              (and (eq action ':assess) (non-empty-string-p identifier)
                   (= (length options) 6)
                   (every (lambda (key) (member key '(:revision :verdict :evidence)))
                          (plist-keys options))
                   (integerp (getf options :revision)) (plusp (getf options :revision))
                   (member (getf options :verdict) '(:passed :failed))
                   (non-empty-string-p (getf options :evidence))))
    (refinement--reject ':invalid "Use (refine :assess ID :revision N :verdict :passed-or-failed :evidence TEXT)."))
  (application-present
   application
   (application-lisp-value-entry
    "refinements"
    (if (null action)
        (refinement-list (application-configuration application))
        (refinement-assess
         (application-operation--tool-context application)
         :identifier identifier :revision (getf options :revision)
         :reviewer (lambda (proposal)
                     (declare (ignore proposal))
                     (values (getf options :verdict) (getf options :evidence)))))))
  nil)

(define-application-command application--builtin-refine-command
    (:name              "refine"
     :argument          "[(:assess ID REVISION VERDICT EVIDENCE)]"
     :description       "inspect refinement proposals or assess task efficacy"
     :tip               "records a local task-level assessment before scoped promotion."
     :busy-behavior     :hold
     :terminal-behavior :shared
     :callable          t)
    (application &optional action identifier &rest options)
  (apply #'application-refinement-command application action identifier options)
  ':continue)
