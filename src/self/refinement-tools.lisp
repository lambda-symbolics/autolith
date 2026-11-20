(in-package #:autolith)

;;;; -- Local Task Assessment --

(-> application-refinement-slash-arguments (string) list)
(defun application-refinement-slash-arguments (remainder)
  "Translate the legacy local assessment form only at the slash boundary."
  (if (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) remainder)))
      nil
      (let ((fields (handler-case
                        (read-one-form remainder :read-eval nil :package (find-package '#:autolith))
                      (error ()
                        (refinement--reject ':invalid "Expected exactly one assessment data form.")))))
        (unless (and (proper-list-p fields) (= (length fields) 5)
                     (eq (first fields) ':assess))
          (refinement--reject ':invalid "Expected (:assess ID REVISION VERDICT EVIDENCE)."))
        (list ':assess (second fields) :revision (third fields)
              :verdict (fourth fields) :evidence (fifth fields)))))

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
    (:name              "/refine"
     :argument          "[(:assess ID REVISION VERDICT EVIDENCE)]"
     :description       "inspect refinement proposals or assess task efficacy"
     :tip               "records a local task-level assessment before scoped promotion."
     :busy-behavior     :hold
     :terminal-behavior :shared
     :slash-parser      application-refinement-slash-arguments
     :callable          t)
    (application &optional action identifier &rest options)
  (apply #'application-refinement-command application action identifier options)
  ':continue)
