(in-package #:autolith)

;;;; -- Procedures --

;;; Procedures are Lisp functions stored in the knowledge log of a workspace.
;;; The source forms contain keyword parameters and documentation strings.

(-> procedure--lambda-list-valid-p (t) boolean)
(defun procedure--lambda-list-valid-p (lambda-list)
  "Return true if LAMBDA-LIST is a valid keyword-only lambda list."
  (and (proper-list-p lambda-list)
       (or (null lambda-list) (eq (first lambda-list) '&key))
       (handler-case
           (multiple-value-bind (required optional rest keywords allow-other-keys auxiliary)
               (parse-ordinary-lambda-list lambda-list)
             (declare (ignore allow-other-keys))
             (let ((names (append
                           (loop for specification in keywords
                               append (cons (second (first specification))
                                            (when (third specification)
                                              (list (third specification)))))
                           (mapcar #'first auxiliary))))
               (and (null required)
                    (null optional)
                    (null rest)
                    (every (lambda (name)
                             (and (symbolp name)
                                  (not (constantp name))
                                  (not (member name lambda-list-keywords))))
                           names)
                    (= (length names) (length (remove-duplicates names)))
                    t)))
         (program-error () nil))))

(-> procedure-form (string) list)
(defun procedure-form (source)
  "Return the LAMBDA form read from SOURCE.

Signal KNOWLEDGE-ERROR if SOURCE is not a LAMBDA form with keyword parameters,
a documentation string and a body."
  (let ((form (handler-case (self-read-form source :read-eval nil)
                (error (condition)
                  (knowledge--fail ':invalid-procedure source
                                   "The source cannot be read: ~A" condition)))))
    (unless (and (proper-list-p form)
                 (>= (length form) 4)
                 (eq (first form) 'lambda)
                 (procedure--lambda-list-valid-p (second form))
                 (stringp (third form)))
      (knowledge--fail ':invalid-procedure source
                       "The source must be a LAMBDA form with keyword parameters, a documentation string and a body."))
    form))

(-> procedure-signature (list) string)
(defun procedure-signature (entry)
  "Return the name and the lambda list of the procedure ENTRY, such as \"report (&key quarter)\"."
  (let ((*package*    (find-package '#:autolith))
        (*print-case* :downcase))
    (format nil "~A ~S" (getf entry :id) (second (procedure-form (getf entry :source))))))

(-> procedure-documentation (list) string)
(defun procedure-documentation (entry)
  "Return the documentation string of the procedure ENTRY."
  (third (procedure-form (getf entry :source))))


;;;; -- Stored Procedures --

(-> procedure-record-valid-p (t) boolean)
(defun procedure-record-valid-p (record)
  "Return true if RECORD is a well-formed procedure record.

Step-procedure records are accepted until the next knowledge-log format migration."
  (let ((properties (rest record)))
    (and (eq (getf properties :scope) ':workspace)
         (non-empty-string-p (getf properties :workspace))
         (typep (getf properties :time) 'timestamp)
         (case (first record)
           (:lisp-procedure-defined
            (and (knowledge-rule-name-p (getf properties :id))
                 (stringp (getf properties :source))))
           (:lisp-procedure-retired
            (knowledge-rule-name-p (getf properties :id)))
           ((:procedure-defined :procedure-admitted :procedure-retired :procedure-run)
            (or (getf properties :procedure)
                (knowledge-rule-name-p (getf properties :id)))))
         t)))

(-> procedure-entry-reduce ((option list) list) (option list))
(defun procedure-entry-reduce (entry record)
  "Return the procedure ENTRY after RECORD without modifying ENTRY.

An entry contains :ID, :SOURCE, :TIME and :RETIRED-P. Returns NIL if RECORD
belongs to a step procedure."
  (let ((properties (rest record)))
    (case (first record)
      (:lisp-procedure-defined
       (when (getf properties :source)
         (list :id        (getf properties :id)
               :source    (getf properties :source)
               :time      (getf properties :time)
               :retired-p nil)))
      (:lisp-procedure-retired
       (when entry
         (let ((entry (copy-list entry)))
           (setf (getf entry :retired-p) t)
           entry))))))

(-> knowledge-base-procedure (knowledge-base string string) (option list))
(defun knowledge-base-procedure (base workspace identifier)
  "Return the procedure entry named IDENTIFIER of WORKSPACE in BASE, or NIL."
  (gethash (list workspace identifier) (knowledge-base-procedures-table base)))

(-> knowledge-base-procedures (knowledge-base string) list)
(defun knowledge-base-procedures (base workspace)
  "Return the procedure entries of WORKSPACE in BASE, ordered by name."
  (let ((entries nil))
    (maphash (lambda (key entry)
               (when (equal (first key) workspace)
                 (push entry entries)))
             (knowledge-base-procedures-table base))
    (sort entries #'string< :key (lambda (entry) (getf entry :id)))))

(-> procedure-define
    (configuration string string &key (:source-conversation (option string)))
    null)
(defun procedure-define (configuration identifier source &key source-conversation)
  "Record SOURCE as the procedure IDENTIFIER of the workspace of CONFIGURATION.

Signal KNOWLEDGE-ERROR if IDENTIFIER is not a valid name or SOURCE is not a
valid procedure."
  (unless (knowledge-rule-name-p identifier)
    (knowledge--fail ':invalid-procedure identifier
                     "A procedure name contains only lowercase letters, digits, hyphens and dots."))
  (procedure-form source)
  (knowledge-transact
   configuration
   (lambda (base)
     (declare (ignore base))
     (values (list (list :lisp-procedure-defined
                         :version             *knowledge-format-version*
                         :scope               ':workspace
                         :workspace           (knowledge--workspace configuration)
                         :id                  identifier
                         :source              source
                         :time                (get-universal-time)
                         :source-conversation source-conversation))
             nil
             t)))
  nil)

(-> procedure-retire (configuration string) boolean)
(defun procedure-retire (configuration identifier)
  "Retire the procedure IDENTIFIER until it is defined again.

Return true if it was not already retired."
  (let ((workspace (knowledge--workspace configuration)))
    (knowledge-transact
     configuration
     (lambda (base)
       (let ((entry (knowledge-base-procedure base workspace identifier)))
         (unless entry
           (knowledge--fail ':unknown-procedure identifier "No procedure is named ~A." identifier))
         (if (getf entry :retired-p)
             (values nil nil nil)
             (values (list (list :lisp-procedure-retired
                                :scope     ':workspace
                                :workspace workspace
                                :id        identifier
                                :time      (get-universal-time)))
                    t
                    t)))))))
