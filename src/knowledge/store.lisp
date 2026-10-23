(in-package #:autolith)

;;;; -- Knowledge Log --

;;; Structured knowledge is an append-only log beside the memory log. Older
;;; Autolith generations never read it, so a rollback keeps every prose memory
;;; readable. Records define schemas, record evidence, assert supports for
;;; claims, and retract supports. Reading folds the log into a KNOWLEDGE-BASE
;;; with indexes; reads are cached by the log file's identity, because every
;;; publication replaces the file.

(defparameter *knowledge-format-version* 1
  "The readable knowledge log format version.")

(defparameter *knowledge-excerpt-limit* 400
  "The maximum characters of evidence text retained in the log.")

(defparameter *knowledge-reason-limit* 400
  "The maximum characters of a retraction reason.")

(defparameter *knowledge-diagnosis-case-limit* 20
  "The most recent diagnosis cases kept per workspace for listing.")

(deftype knowledge-basis ()
  "Why a support is believed."
  '(member :observation :user-assertion :assumption :model-proposal))

(defvar *knowledge-lock* (make-recursive-lock "Autolith structured knowledge")
  "Serialize same-process knowledge transactions.")

(defvar *knowledge-base-cache* (make-hash-table :test #'equal :synchronized t)
  "(IDENTITY SIZE POSITION BASE INCOMPLETE-P) of each folded log by namestring.

IDENTITY and SIZE describe the file BASE reflects, POSITION is where reading
stopped, and INCOMPLETE-P whether an incomplete final record follows it.")

(defclass knowledge-base ()
  ((revision
    :initform 0
    :accessor knowledge-base-revision
    :type (integer 0)
    :documentation "How many records the log held; each record is one graph revision.")
   (schemas
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-schemas
    :type hash-table
    :documentation "(SCOPE WORKSPACE PREDICATE) to (SCHEMA VERSION).")
   (evidence
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-evidence
    :type hash-table
    :documentation "Evidence records by identifier.")
   (supports
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-supports
    :type hash-table
    :documentation "Support records by identifier, retracted ones included.")
   (claims
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-claims
    :type hash-table
    :documentation "Claim plists by identifier: proposition, scope, workspace, supports.")
   (predicate-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-predicate-index
    :type hash-table
    :documentation "Claim identifiers by predicate name.")
   (term-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-term-index
    :type hash-table
    :documentation "Claim identifiers by each string argument, for entity lookup.")
   (evidence-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-evidence-index
    :type hash-table
    :documentation "Support identifiers by the evidence they rest on.")
   (rules
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-rules
    :type hash-table
    :documentation "Rule entries by (SCOPE WORKSPACE IDENTIFIER): versions and the active one.")
   (methods
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-methods-table
    :type hash-table
    :documentation "Task method entries by (SCOPE WORKSPACE IDENTIFIER): the latest definition.")
   (contracts
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-contracts-table
    :type hash-table
    :documentation "Action contract entries by (SCOPE WORKSPACE IDENTIFIER): the latest definition.")
   (procedures
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-procedures-table
    :type hash-table
    :documentation "Learned procedure entries by (WORKSPACE IDENTIFIER): versions, admission, and runs.")
   (diagnoses
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-diagnoses-table
    :type hash-table
    :documentation "Saved diagnosis entries by (SCOPE WORKSPACE IDENTIFIER): the latest definition.")
   (diagnosis-cases
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-diagnosis-cases-table
    :type hash-table
    :documentation "Each workspace's recent diagnosis cases, newest first."))
  (:documentation "The folded state of the knowledge log, with lookup indexes."))


;;;; -- Activation --

(-> knowledge-mode (configuration) keyword)
(defun knowledge-mode (configuration)
  "Return CONFIGURATION's structured knowledge mode: :ACTIVE, :PASSIVE, or :OFF."
  (let ((mode (config :knowledge-mode configuration)))
    (cond
      ((equal mode "passive") ':passive)
      ((equal mode "off") ':off)
      (t ':active))))

(-> knowledge-active-p ((option configuration)) boolean)
(defun knowledge-active-p (configuration)
  "Return true when facts and rules are offered to the model under CONFIGURATION.

Without a configuration, as in standalone tool registries, knowledge is active."
  (or (null configuration) (eq (knowledge-mode configuration) ':active)))

(-> knowledge-capture-p (configuration) boolean)
(defun knowledge-capture-p (configuration)
  "Return true when observations are recorded as facts under CONFIGURATION."
  (and (member (knowledge-mode configuration) '(:active :passive)) t))


;;;; -- Record Validation --

(-> knowledge--scope-valid-p (t t) boolean)
(defun knowledge--scope-valid-p (scope workspace)
  "Return true when SCOPE and WORKSPACE describe a global or workspace owner."
  (and (typep scope 'memory-scope)
       (if (eq scope ':workspace)
           (non-empty-string-p workspace)
           (null workspace))
       t))

(-> knowledge-evidence-source-p (t) boolean)
(defun knowledge-evidence-source-p (source)
  "Return true when SOURCE is a portable evidence source description."
  (and (proper-list-p source)
       (evenp (length (rest source)))
       (let ((properties (rest source)))
         (flet ((optional-text-p (key limit)
                  (let ((value (getf properties key)))
                    (or (null value) (and (stringp value) (<= (length value) limit))))))
           (case (first source)
             (:file
              (and (non-empty-string-p (getf properties :path))
                   (member (getf properties :relative) '(nil t))
                   (optional-text-p :quote *memory-quote-limit*)))
             (:url
              (and (non-empty-string-p (getf properties :url))
                   (<= (length (getf properties :url)) *memory-source-url-limit*)
                   (optional-text-p :quote *memory-quote-limit*)))
             (:user
              (and (memory-source-quote-p (getf properties :quote))
                   (optional-text-p :conversation 200)))
             (:tool
              (and (non-empty-string-p (getf properties :name))
                   (optional-text-p :conversation 200)
                   (optional-text-p :call 200)))
             (:memory
              (and (non-empty-string-p (getf properties :id))
                   (memory-source-digest-p (getf properties :digest))))
             ((:mission :skill)
              (and (non-empty-string-p (getf properties :id))
                   (optional-text-p :detail 200)))
             (otherwise
              nil))))
       t))

(-> knowledge--evidence-record-p (list) boolean)
(defun knowledge--evidence-record-p (record)
  "Return true when RECORD is a complete evidence record."
  (let ((properties (rest record)))
    (and (non-empty-string-p (getf properties :id))
         (knowledge-evidence-source-p (getf properties :source))
         (or (null (getf properties :workspace))
             (non-empty-string-p (getf properties :workspace)))
         (or (null (getf properties :digest))
             (memory-source-digest-p (getf properties :digest)))
         (typep (getf properties :observed-at) 'timestamp)
         (or (null (getf properties :valid-until))
             (typep (getf properties :valid-until) 'timestamp))
         (or (null (getf properties :excerpt))
             (and (stringp (getf properties :excerpt))
                  (<= (length (getf properties :excerpt)) *knowledge-excerpt-limit*)))
         (non-empty-string-p (getf properties :adapter))
         t)))

(-> knowledge--support-record-p (list) boolean)
(defun knowledge--support-record-p (record)
  "Return true when RECORD is a complete claim assertion record."
  (let ((properties (rest record)))
    (and (non-empty-string-p (getf properties :support))
         (non-empty-string-p (getf properties :claim))
         (knowledge--scope-valid-p (getf properties :scope) (getf properties :workspace))
         (proper-list-p (getf properties :proposition))
         (or (null (getf properties :evidence))
             (non-empty-string-p (getf properties :evidence)))
         (typep (getf properties :basis) 'knowledge-basis)
         (or (getf properties :evidence)
             (member (getf properties :basis) '(:assumption :model-proposal)))
         (or (null (getf properties :valid-from))
             (typep (getf properties :valid-from) 'timestamp))
         (or (null (getf properties :valid-until))
             (typep (getf properties :valid-until) 'timestamp))
         (typep (getf properties :time) 'timestamp)
         t)))

(-> knowledge--properties-p (t) boolean)
(defun knowledge--properties-p (properties)
  "Return true for a proper property list with unique keyword names."
  (and (proper-list-p properties)
       (evenp (length properties))
       (loop with seen = nil
             for (key value) on properties by #'cddr
             always (and (keywordp key)
                         (not (member key seen))
                         (progn (push key seen) t)))))

(-> knowledge--record-valid-p (t) boolean)
(defun knowledge--record-valid-p (record)
  "Return true when RECORD is well formed. Unknown kinds are left to the reducer."
  (and (proper-list-p record)
       (keywordp (first record))
       (knowledge--properties-p (rest record))
       (case (first record)
         (:schema-defined
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-schema-p (getf properties :schema))
                 (typep (getf properties :time) 'timestamp))))
         (:evidence
          (knowledge--evidence-record-p record))
         (:evidence-renewed
          (let ((properties (rest record)))
            (and (non-empty-string-p (getf properties :id))
                 (typep (getf properties :observed-at) 'timestamp)
                 (or (null (getf properties :valid-until))
                     (typep (getf properties :valid-until) 'timestamp)))))
         (:claim-asserted
          (knowledge--support-record-p record))
         (:support-retracted
          (let ((properties (rest record)))
            (and (non-empty-string-p (getf properties :support))
                 (typep (getf properties :time) 'timestamp)
                 (or (null (getf properties :reason))
                     (and (stringp (getf properties :reason))
                          (<= (length (getf properties :reason))
                              *knowledge-reason-limit*))))))
         (:rule-defined
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-rule-p (getf properties :rule))
                 (typep (getf properties :time) 'timestamp))))
         ((:rule-admitted :rule-retired)
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-rule-name-p (getf properties :id))
                 (or (eq (first record) :rule-retired)
                     (typep (getf properties :rule-version) '(integer 1)))
                 (typep (getf properties :time) 'timestamp))))
         (:method-defined
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-method-p (getf properties :method))
                 (typep (getf properties :time) 'timestamp))))
         (:contract-defined
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-contract-p (getf properties :contract))
                 (or (null (getf properties :contract-version))
                     (typep (getf properties :contract-version) '(integer 1)))
                 (let ((admission (getf properties :admission)))
                   (or (null admission)
                       (and (knowledge--properties-p admission)
                            (knowledge-contract-p (getf admission :contract))
                            (equal (getf (rest (getf admission :contract)) :id)
                                   (getf (rest (getf properties :contract)) :id))
                            (typep (getf properties :contract-version) '(integer 1))
                            (typep (getf admission :contract-version) '(integer 1))
                            (<= (getf admission :contract-version)
                                (getf properties :contract-version))
                            (non-empty-string-p (getf admission :by))
                            (typep (getf admission :time) 'timestamp))))
                 (typep (getf properties :time) 'timestamp))))
         ((:procedure-defined :procedure-admitted :procedure-retired :procedure-run)
          (procedure-record-valid-p record))
         (:diagnosis-defined
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-diagnosis-p (getf properties :diagnosis))
                 (typep (getf properties :time) 'timestamp))))
         (:diagnosis-recorded
          (let* ((properties (rest record))
                 (case (getf properties :case)))
            (and (eq (getf properties :scope) ':workspace)
                 (non-empty-string-p (getf properties :workspace))
                 (proper-list-p case)
                 (evenp (length case))
                 (stringp (getf case :diagnosis))
                 (proper-list-p (getf case :remaining))
                 (proper-list-p (getf case :eliminated))
                 (typep (getf properties :time) 'timestamp))))
         (:transaction
          (let ((records (getf (rest record) :records)))
            (and (proper-list-p records)
                 records
                 (every (lambda (inner)
                          (and (consp inner)
                               (not (eq (first inner) :transaction))
                               (knowledge--record-valid-p inner)))
                        records))))
         (otherwise
          t))
       t))


;;;; -- Folding --

(-> knowledge--index-add (hash-table t string) null)
(defun knowledge--index-add (index key identifier)
  "Add IDENTIFIER under KEY in INDEX once."
  (pushnew identifier (gethash key index) :test #'string=)
  nil)

(-> knowledge--reduce (pathname knowledge-base list) knowledge-base)
(defun knowledge--reduce (pathname base record)
  "Fold one validated RECORD into fresh BASE.

Records of kinds this generation does not know are skipped, so a log written
by a newer generation stays readable after a rollback."
  (flet ((fail (message)
           (error 'knowledge-error
                  :code ':corrupt
                  :subject (namestring pathname)
                  :message message)))
    (let ((properties (rest record)))
      (case (first record)
        (:schema-defined
         (let* ((schema (getf properties :schema))
                (key (list (getf properties :scope)
                           (getf properties :workspace)
                           (knowledge-schema-predicate schema)))
                (previous (gethash key (knowledge-base-schemas base))))
           (setf (gethash key (knowledge-base-schemas base))
                 (list schema (if previous (1+ (second previous)) 1)))))
        (:evidence
         (setf (gethash (getf properties :id) (knowledge-base-evidence base))
               (copy-list properties)))
        (:evidence-renewed
         (let ((evidence (copy-list (gethash (getf properties :id)
                                             (knowledge-base-evidence base)))))
           (unless evidence
             (fail (format nil "A renewal names unknown evidence ~A." (getf properties :id))))
           (setf (getf evidence :observed-at) (getf properties :observed-at)
                 (getf evidence :valid-until) (getf properties :valid-until)
                 (gethash (getf properties :id) (knowledge-base-evidence base)) evidence)))
        (:claim-asserted
         (let ((claim (getf properties :claim))
               (support (getf properties :support))
               (evidence (getf properties :evidence)))
           (when (and evidence
                      (null (gethash evidence (knowledge-base-evidence base))))
             (fail (format nil "Support ~A cites unknown evidence ~A." support evidence)))
           (setf (gethash support (knowledge-base-supports base))
                 (copy-list properties))
           (let ((entry (gethash claim (knowledge-base-claims base))))
             (if entry
                 (let ((copy (copy-list entry)))
                   (setf (getf copy :supports)
                         (adjoin support (getf entry :supports) :test #'string=)
                         (gethash claim (knowledge-base-claims base))
                         copy))
                 (let ((proposition (getf properties :proposition)))
                   (setf (gethash claim (knowledge-base-claims base))
                         (list :id claim
                               :proposition proposition
                               :scope (getf properties :scope)
                               :workspace (getf properties :workspace)
                               :supports (list support)))
                   (knowledge--index-add (knowledge-base-predicate-index base)
                                         (knowledge-proposition-predicate proposition)
                                         claim)
                   (dolist (term (knowledge-proposition-arguments proposition))
                     (when (stringp term)
                       (knowledge--index-add (knowledge-base-term-index base)
                                             term claim))))))
           (when evidence
             (knowledge--index-add (knowledge-base-evidence-index base) evidence support))))
        (:support-retracted
         (let ((support (gethash (getf properties :support)
                                 (knowledge-base-supports base))))
           (unless support
             (fail (format nil "Retraction names unknown support ~A."
                           (getf properties :support))))
           (setf (getf support :retracted-at) (getf properties :time)
                 (getf support :retraction-reason) (getf properties :reason)
                 (gethash (getf properties :support) (knowledge-base-supports base))
                 support)))
        (:rule-defined
         (let* ((rule (getf properties :rule))
                (key (list (getf properties :scope) (getf properties :workspace)
                           (getf (rest rule) :id)))
                (entry (or (copy-list (gethash key (knowledge-base-rules base)))
                           (list :id (getf (rest rule) :id)
                                 :scope (getf properties :scope)
                                 :workspace (getf properties :workspace)
                                 :versions nil
                                 :active nil))))
           (setf (getf entry :versions)
                 (append (getf entry :versions)
                         (list (list :version (1+ (length (getf entry :versions)))
                                     :rule rule
                                     :time (getf properties :time)
                                     :source-conversation
                                     (getf properties :source-conversation))))
                 (gethash key (knowledge-base-rules base)) entry)))
        ((:rule-admitted :rule-retired)
         (let* ((key (list (getf properties :scope) (getf properties :workspace)
                           (getf properties :id)))
                (entry (copy-list (gethash key (knowledge-base-rules base)))))
           (unless (and entry
                        (or (eq (first record) :rule-retired)
                            (<= 1 (getf properties :rule-version)
                                (length (getf entry :versions)))))
             (fail (format nil "~A names unknown rule ~A." (first record)
                           (getf properties :id))))
           (setf (getf entry :active) (and (eq (first record) :rule-admitted)
                                           (getf properties :rule-version))
                 (gethash key (knowledge-base-rules base)) entry)))
        (:method-defined
         (let ((method (getf properties :method)))
           (setf (gethash (list (getf properties :scope) (getf properties :workspace)
                                (getf (rest method) :id))
                          (knowledge-base-methods-table base))
                 (list :method method
                       :scope (getf properties :scope)
                       :workspace (getf properties :workspace)
                       :time (getf properties :time)))))
        (:contract-defined
         (let* ((contract (getf properties :contract))
                (key (list (getf properties :scope) (getf properties :workspace)
                           (getf (rest contract) :id)))
                (previous (gethash key (knowledge-base-contracts-table base)))
                ;; An unadmitted proposal cannot retire a local authorization or
                ;; move the local version backwards when importing older history.
                (version (max (or (getf properties :contract-version) 1)
                              (or (getf previous :contract-version) 1)))
                (admission (or (getf properties :admission)
                               (getf previous :admission))))
           (unless (or (null admission)
                       (and (eql version (getf admission :contract-version))
                            (equal contract (getf admission :contract)))
                       (equal admission (getf previous :admission)))
             (fail "A contract proposal carries an admission for an unknown earlier definition."))
           (setf (gethash key (knowledge-base-contracts-table base))
                 (list :contract contract :contract-version version :admission admission
                       :scope (getf properties :scope)
                       :workspace (getf properties :workspace)
                       :time (getf properties :time)))))
        ((:procedure-defined :procedure-admitted :procedure-retired :procedure-run)
         (let ((key (list (getf properties :workspace)
                          (if (eq (first record) :procedure-defined)
                              (getf (rest (getf properties :procedure)) :id)
                              (getf properties :id)))))
           (setf (gethash key (knowledge-base-procedures-table base))
                 (procedure-entry-reduce (gethash key (knowledge-base-procedures-table base))
                                         record))))
        (:diagnosis-defined
         (let ((diagnosis (getf properties :diagnosis)))
           (setf (gethash (list (getf properties :scope) (getf properties :workspace)
                                (getf (rest diagnosis) :id))
                          (knowledge-base-diagnoses-table base))
                 (list :diagnosis diagnosis
                       :scope (getf properties :scope)
                       :workspace (getf properties :workspace)
                       :time (getf properties :time)))))
        (:diagnosis-recorded
         (let ((cases (cons (append (getf properties :case)
                                    (list :time (getf properties :time)
                                          :conversation (getf properties :conversation)))
                            (gethash (getf properties :workspace)
                                     (knowledge-base-diagnosis-cases-table base)))))
           (setf (gethash (getf properties :workspace) (knowledge-base-diagnosis-cases-table base))
                 (subseq cases 0 (min (length cases) *knowledge-diagnosis-case-limit*)))))
        (:transaction
         ;; One atomically appended batch; its records are the revisions.
         (dolist (inner (getf properties :records))
           (knowledge--reduce pathname base inner))
         (decf (knowledge-base-revision base)))
        (otherwise
         ;; A record kind from a newer generation: skip it.
         nil)))
    (incf (knowledge-base-revision base))
    base))

(-> knowledge--copy-base (knowledge-base) knowledge-base)
(defun knowledge--copy-base (base)
  "Return a new base sharing BASE's entries but not its tables.

The reducer replaces entries instead of modifying them, so folding new records
into the copy leaves BASE, which readers may still be using, unchanged."
  (let ((copy (make-instance 'knowledge-base)))
    (setf (knowledge-base-revision copy) (knowledge-base-revision base))
    (loop for (source target) in (list (list #'knowledge-base-schemas #'knowledge-base-schemas)
                                       (list #'knowledge-base-evidence #'knowledge-base-evidence)
                                       (list #'knowledge-base-supports #'knowledge-base-supports)
                                       (list #'knowledge-base-claims #'knowledge-base-claims)
                                       (list #'knowledge-base-predicate-index
                                             #'knowledge-base-predicate-index)
                                       (list #'knowledge-base-term-index #'knowledge-base-term-index)
                                       (list #'knowledge-base-evidence-index
                                             #'knowledge-base-evidence-index)
                                       (list #'knowledge-base-rules #'knowledge-base-rules)
                                       (list #'knowledge-base-methods-table
                                             #'knowledge-base-methods-table)
                                       (list #'knowledge-base-contracts-table
                                             #'knowledge-base-contracts-table)
                                       (list #'knowledge-base-procedures-table
                                             #'knowledge-base-procedures-table)
                                       (list #'knowledge-base-diagnoses-table
                                             #'knowledge-base-diagnoses-table)
                                       (list #'knowledge-base-diagnosis-cases-table
                                             #'knowledge-base-diagnosis-cases-table))
          do (let ((into (funcall target copy)))
               (maphash (lambda (key value) (setf (gethash key into) value))
                        (funcall source base))))
    copy))

(-> knowledge--header () list)
(defun knowledge--header ()
  "Return the header that starts every knowledge log."
  (list :knowledge :version *knowledge-format-version*))

(-> knowledge--lock-pathname (configuration) pathname)
(defun knowledge--lock-pathname (configuration)
  "Return the advisory lock serializing writers of CONFIGURATION's knowledge log."
  (readable-state-lock-pathname (configuration-knowledge-path configuration) "knowledge.lock"))

(-> knowledge--fold-from (pathname knowledge-base integer)
    (values knowledge-base integer boolean))
(defun knowledge--fold-from (pathname base position)
  "Fold PATHNAME's complete records after byte POSITION into BASE.

Return BASE, the position after the last complete record, and whether an
incomplete final record remains. Position zero starts with the header."
  (let ((header-p (zerop position)))
    (multiple-value-bind (next incomplete-p)
        (sexp-store:log-map
         (lambda (form)
           (if header-p
               (progn
                 (unless (sexp-store:record-check form :tag ':knowledge
                                                       :versions (list *knowledge-format-version*))
                   (knowledge--fail ':corrupt (namestring pathname)
                                    "The knowledge log header is not supported."))
                 (setf header-p nil))
               (progn
                 (unless (knowledge--record-valid-p form)
                   (knowledge--fail ':corrupt (namestring pathname)
                                    "A complete knowledge record is malformed."))
                 (knowledge--reduce pathname base form))))
         pathname
         :start-position position)
      (values base next incomplete-p))))

(-> knowledge--read-current (configuration) (values knowledge-base boolean))
(defun knowledge--read-current (configuration)
  "Return CONFIGURATION's current base, folding only records added since the cached read.

The second value is true when the log ends in an incomplete record. Appends
keep the file's identity, so the cache continues from its position; any other
replacement rereads the whole log."
  (let* ((pathname (configuration-knowledge-path configuration))
         (name (namestring pathname))
         (status (platform-path-status *platform* pathname :follow-links-p t)))
    (if (null status)
        (values (make-instance 'knowledge-base) nil)
        (with-recursive-lock-held (*knowledge-lock*)
          (destructuring-bind (&optional identity size position base incomplete-p)
              (gethash name *knowledge-base-cache*)
            (let ((same-p (and base (equal identity (platform-file-status-identity status)))))
              (cond
                ((and same-p (= size (platform-file-status-size status)))
                 (values base incomplete-p))
                (t
                 (multiple-value-bind (folded next incomplete-p)
                     (if (and same-p (< size (platform-file-status-size status)))
                         (knowledge--fold-from pathname (knowledge--copy-base base) position)
                         (knowledge--fold-from pathname (make-instance 'knowledge-base) 0))
                   (setf (gethash name *knowledge-base-cache*)
                         (list (platform-file-status-identity status)
                               (platform-file-status-size status)
                               next folded incomplete-p))
                   (values folded incomplete-p))))))))))

(-> knowledge--translate-store-errors (function) t)
(defun knowledge--translate-store-errors (function)
  "Call FUNCTION, reporting storage failures as KNOWLEDGE-ERROR."
  (handler-case (funcall function)
    (sexp-store:store-error (cause)
      (error 'knowledge-error
             :code ':storage
             :subject (namestring (sexp-store:store-error-pathname cause))
             :message (sexp-store:store-error-message cause)))))


;;;; -- Reading and Transactions --

(-> knowledge-base-read (configuration) knowledge-base)
(defun knowledge-base-read (configuration)
  "Return CONFIGURATION's folded knowledge base, reading only what was appended.

The returned base is shared; callers must not modify it."
  (values (knowledge--translate-store-errors
           (lambda () (knowledge--read-current configuration)))))

(-> knowledge-transact (configuration function) t)
(defun knowledge-transact (configuration update)
  "Apply UPDATE to the current knowledge base and append its records atomically.

UPDATE receives the shared current base, which it must not modify, and
returns RECORDS, RESULT, and WRITE-P. The records are validated and folded
before anything is written; several records are appended as one transaction,
so an interrupted write leaves none of them. Return RESULT."
  (let ((pathname (configuration-knowledge-path configuration)))
    (knowledge--translate-store-errors
     (lambda ()
       (with-recursive-lock-held (*knowledge-lock*)
         (call-with-file-lock
          (knowledge--lock-pathname configuration)
          (lambda ()
            (multiple-value-bind (base incomplete-p) (knowledge--read-current configuration)
              (when incomplete-p
                (sexp-store:log-repair-tail pathname)
                (setf base (knowledge--read-current configuration)))
              (multiple-value-bind (records result write-p) (funcall update base)
                (when (and write-p records)
                  (let ((form (if (rest records)
                                  (list :transaction :version *knowledge-format-version*
                                        :records records)
                                  (first records))))
                    (unless (knowledge--record-valid-p form)
                      (knowledge--fail ':invalid form "A knowledge record is malformed."))
                    (knowledge--reduce pathname (knowledge--copy-base base) form)
                    (sexp-store:log-append pathname form
                                           :initial-forms (list (knowledge--header))
                                           :repair-tail-p nil)))
                result)))))))))


;;;; -- Lookup --

(defparameter *knowledge-builtin-schemas*
  (list (knowledge-make-schema
         :predicate "autolith.mission.status"
         :arguments '((:name "mission" :type :entity) (:name "status" :type :string))
         :key '("mission")
         :template "mission {mission} is {status}"
         :description "The latest status of an Autolith mission.")
        (knowledge-make-schema
         :predicate "autolith.mission.gate"
         :arguments '((:name "mission" :type :entity) (:name "gate" :type :string)
                      (:name "status" :type :string))
         :key '("mission" "gate")
         :template "gate {gate} of mission {mission} {status}"
         :description "The latest result of one mission acceptance gate."))
  "Global predicates Autolith's own observations use, defined by Autolith itself.

Their names start with autolith. so they never collide with a vocabulary a
person or model defines.")

(-> knowledge-base-schema (knowledge-base string &key (:workspace (option string)))
    (values (option list) (option integer)))
(defun knowledge-base-schema (base predicate &key workspace)
  "Return PREDICATE's schema visible from WORKSPACE and its version, or NIL.

A workspace definition shadows a global one, and Autolith's built-in
predicates, version 0, are visible everywhere."
  (let ((entry (or (and workspace
                        (gethash (list ':workspace workspace predicate)
                                 (knowledge-base-schemas base)))
                   (gethash (list ':global nil predicate)
                            (knowledge-base-schemas base)))))
    (if entry
        (values (first entry) (second entry))
        (let ((builtin (find predicate *knowledge-builtin-schemas*
                             :key #'knowledge-schema-predicate :test #'string=)))
          (values builtin (and builtin 0))))))

(-> knowledge-visible-p (memory-scope (option string) (option string)) boolean)
(defun knowledge-visible-p (scope workspace viewer)
  "Return true when knowledge owned by SCOPE and WORKSPACE applies in workspace VIEWER."
  (and (or (eq scope ':global) (equal workspace viewer)) t))

(-> knowledge-base-claim (knowledge-base string) (option list))
(defun knowledge-base-claim (base identifier)
  "Return claim IDENTIFIER's plist, or NIL."
  (values (gethash identifier (knowledge-base-claims base))))

(-> knowledge-base-support (knowledge-base string) (option list))
(defun knowledge-base-support (base identifier)
  "Return support IDENTIFIER's plist, retracted or not, or NIL."
  (values (gethash identifier (knowledge-base-supports base))))

(-> knowledge-base-evidence-record (knowledge-base string) (option list))
(defun knowledge-base-evidence-record (base identifier)
  "Return evidence IDENTIFIER's plist, or NIL."
  (values (gethash identifier (knowledge-base-evidence base))))

(-> knowledge-base-claims-with-predicate (knowledge-base string) list)
(defun knowledge-base-claims-with-predicate (base predicate)
  "Return the identifiers of claims about PREDICATE."
  (values (gethash predicate (knowledge-base-predicate-index base))))

(-> knowledge-base-claims-mentioning (knowledge-base string) list)
(defun knowledge-base-claims-mentioning (base term)
  "Return the identifiers of claims with string argument TERM, such as an entity."
  (values (gethash term (knowledge-base-term-index base))))


;;;; -- Writing --

(-> knowledge--workspace (configuration) string)
(defun knowledge--workspace (configuration)
  "Return CONFIGURATION's workspace identity, as memories record it."
  (namestring (config :working-directory configuration)))

(-> knowledge--owner (configuration memory-scope) (option string))
(defun knowledge--owner (configuration scope)
  "Return the workspace owning knowledge in SCOPE, or NIL for global knowledge."
  (and (eq scope ':workspace) (knowledge--workspace configuration)))

(-> knowledge-define-schema
    (configuration list &key (:scope memory-scope) (:source-conversation (option string)))
    (values list integer))
(defun knowledge-define-schema (configuration schema &key (scope ':workspace) source-conversation)
  "Define or redefine predicate SCHEMA in SCOPE and return it and its version.

Redefining an unchanged schema writes nothing. A redefinition may change the
key, template, and description; changing arguments while claims use the
predicate is refused, because those claims would no longer fit."
  (unless (knowledge-schema-p schema)
    (knowledge--fail ':invalid-schema schema "The schema is malformed."))
  (let ((workspace (knowledge--owner configuration scope))
        (predicate (knowledge-schema-predicate schema)))
    (values-list
     (knowledge-transact
     configuration
     (lambda (base)
       (destructuring-bind (&optional existing version)
           (gethash (list scope workspace predicate) (knowledge-base-schemas base))
         (cond
           ((equal existing schema)
            (values nil (list existing version) nil))
           ((and existing
                 (not (equal (knowledge-schema-arguments existing)
                             (knowledge-schema-arguments schema)))
                 (some (lambda (claim)
                         (let ((entry (knowledge-base-claim base claim)))
                           (and (eq (getf entry :scope) scope)
                                (equal (getf entry :workspace) workspace))))
                       (knowledge-base-claims-with-predicate base predicate)))
            (knowledge--fail ':schema-in-use predicate
                             "Claims already use ~A; keep its arguments, or define a new predicate."
                             predicate))
           (t
            (values (list (list :schema-defined
                                :version *knowledge-format-version*
                                :scope scope
                                :workspace workspace
                                :schema schema
                                :time (get-universal-time)
                                :source-conversation source-conversation))
                    (list schema (if version (1+ version) 1))
                    t)))))))))

(-> knowledge-make-evidence
    (&key (:source list) (:workspace (option string)) (:digest (option string))
          (:observed-at timestamp) (:valid-until (option timestamp))
          (:excerpt (option string)) (:adapter string))
    list)
(defun knowledge-make-evidence
    (&key source workspace digest observed-at valid-until excerpt adapter)
  "Return a validated evidence record for one observation.

The identifier depends on the source, digest, adapter, and workspace, so the
same observation recorded twice is one piece of evidence."
  (let ((record (list :evidence
                      :version *knowledge-format-version*
                      :id (knowledge-digest-identifier
                           #\e (list source digest adapter workspace))
                      :source source
                      :workspace workspace
                      :digest digest
                      :observed-at observed-at
                      :valid-until valid-until
                      :excerpt (and excerpt
                                    (subseq excerpt 0 (min (length excerpt)
                                                           *knowledge-excerpt-limit*)))
                      :adapter adapter)))
    (unless (knowledge--evidence-record-p record)
      (knowledge--fail ':invalid-evidence source "The evidence description is malformed."))
    record))

(-> knowledge--observation-records (knowledge-base (option list)) list)
(defun knowledge--observation-records (base evidence)
  "Return records to retain EVIDENCE or renew a newer observation of its identity."
  (when evidence
    (let* ((properties (rest evidence))
           (identifier (getf properties :id))
           (recorded (knowledge-base-evidence-record base identifier)))
      (cond
        ((null recorded)
         (list evidence))
        ((> (getf properties :observed-at) (getf recorded :observed-at))
         (list (list :evidence-renewed
                     :id identifier
                     :observed-at (getf properties :observed-at)
                     :valid-until (getf properties :valid-until))))))))

(-> knowledge-assert
    (configuration list
     &key (:scope memory-scope) (:evidence (option list)) (:basis knowledge-basis)
          (:valid-from (option timestamp)) (:valid-until (option timestamp))
          (:source-conversation (option string)) (:contract (option list)))
    (values string string))
(defun knowledge-assert
    (configuration proposition
     &key (scope ':workspace) evidence basis valid-from valid-until source-conversation contract)
  "Support the claim PROPOSITION makes in SCOPE and return the claim and support IDs.

PROPOSITION must fit a schema visible from the workspace. EVIDENCE is a record
from KNOWLEDGE-MAKE-EVIDENCE, or NIL for an assumption or a model's own
proposal. Asserting the same support again returns the existing one; when
its evidence was observed again since, the renewed observation time and expiry
are recorded, so a refreshed observation applies again. When CONTRACT is
provided, its exact mapping must still be admitted at the assertion's commit;
a superseded mapping signals INVALID-CONTRACT without writing any records."
  (let* ((workspace (knowledge--owner configuration scope))
         (viewer (knowledge--workspace configuration))
         (claim (knowledge-claim-identifier proposition :scope scope :workspace workspace)))
    (unless (typep basis 'knowledge-basis)
      (knowledge--fail ':invalid-claim basis
                       "Basis must be observation, user-assertion, assumption, or model-proposal."))
    (when (and (null evidence) (not (member basis '(:assumption :model-proposal))))
      (knowledge--fail ':invalid-claim basis
                       "Only an assumption or a model proposal may be asserted without evidence."))
    (when (and valid-from valid-until (> valid-from valid-until))
      (knowledge--fail ':invalid-claim valid-until
                       "A validity interval cannot end before it starts."))
    (values-list
     (knowledge-transact
     configuration
     (lambda (base)
       (when (and contract
                  (not (member contract
                               (knowledge-base-contracts base (getf (rest contract) :tool)
                                                         :workspace viewer)
                               :test #'equal)))
         (knowledge--fail ':invalid-contract (getf (rest contract) :id)
                          "The contract mapping is no longer admitted."))
       (let ((schema (knowledge-base-schema base (knowledge-proposition-predicate proposition)
                                            :workspace viewer)))
         (unless schema
           (knowledge--fail ':unknown-predicate (knowledge-proposition-predicate proposition)
                            "Define predicate ~A before asserting claims about it."
                            (knowledge-proposition-predicate proposition)))
         (when (and (eq scope ':global)
                    (null (knowledge-base-schema base (knowledge-proposition-predicate proposition))))
           (knowledge--fail ':unknown-predicate (knowledge-proposition-predicate proposition)
                            "A global claim needs a global definition of ~A."
                            (knowledge-proposition-predicate proposition)))
         (unless (knowledge-proposition-valid-p proposition schema)
           (knowledge--fail ':invalid-claim (knowledge-proposition-predicate proposition)
                            "The claim does not fit the schema of ~A."
                            (knowledge-proposition-predicate proposition)))
         (let* ((evidence-identifier (and evidence (getf (rest evidence) :id)))
                (existing
                  (find-if (lambda (support-identifier)
                             (let ((support (knowledge-base-support base support-identifier)))
                               (and (null (getf support :retracted-at))
                                    (equal (getf support :evidence) evidence-identifier)
                                    (eq (getf support :basis) basis)
                                    (eql (getf support :valid-from) valid-from)
                                    (eql (getf support :valid-until) valid-until))))
                           (getf (knowledge-base-claim base claim) :supports)))
                (observation-records (knowledge--observation-records base evidence)))
            (if existing
                (values observation-records (list claim existing) (and observation-records t))
                (let ((support (make-identifier)))
                  (values
                   (append
                    observation-records
                    (list (list :claim-asserted
                                :version *knowledge-format-version*
                                :support support
                                :claim claim
                                :scope scope
                                :workspace workspace
                                :proposition proposition
                                :evidence evidence-identifier
                                :basis basis
                                :valid-from valid-from
                                :valid-until valid-until
                                :time (get-universal-time)
                                :source-conversation source-conversation)))
                   (list claim support)
                   t))))))))))

(-> knowledge-retract
    (configuration string &key (:reason (option string)) (:source-conversation (option string)))
    list)
(defun knowledge-retract (configuration support &key reason source-conversation)
  "Retract SUPPORT, or every active support of a claim when SUPPORT is a claim ID.

Return the retracted support identifiers. Retracted supports stay inspectable."
  (when (and reason (> (length reason) *knowledge-reason-limit*))
    (setf reason (subseq reason 0 *knowledge-reason-limit*)))
  (knowledge-transact
   configuration
   (lambda (base)
     (let* ((claim (knowledge-base-claim base support))
            (targets
              (remove-if (lambda (identifier)
                           (getf (knowledge-base-support base identifier) :retracted-at))
                         (cond
                           (claim
                            (getf claim :supports))
                           ((knowledge-base-support base support)
                            (list support))
                           (t
                            (knowledge--fail ':unknown-support support
                                             "No claim or support ~A exists." support)))))
            (time (get-universal-time)))
       (values (mapcar (lambda (identifier)
                         (list :support-retracted
                               :version *knowledge-format-version*
                               :support identifier
                               :reason reason
                               :time time
                               :source-conversation source-conversation))
                       targets)
               targets
               (and targets t))))))

(-> knowledge--source-identity (list) list)
(defun knowledge--source-identity (source)
  "Return SOURCE's stable producer identity, independent of observation details.

Files use their path and relative-path flag; URLs use their address. Tools use
their name, not the conversation or call that recorded an observation. User
statements use their conversation when known, or their quotation otherwise.
Memory, mission, and skill sources use their object ID. Quotations, digests,
and adapter details do not distinguish revisions of an identified source."
  (let ((properties (rest source)))
    (case (first source)
      (:file
       (list ':file (getf properties :path) (getf properties :relative)))
      (:url
       (list ':url (getf properties :url)))
      (:tool
       (list ':tool (getf properties :name)))
      (:user
       (if (getf properties :conversation)
           (list ':user ':conversation (getf properties :conversation))
           (list ':user ':quote (getf properties :quote))))
      ((:memory :mission :skill)
       (list (first source) (getf properties :id)))
      (otherwise
       (knowledge--fail ':invalid-evidence source "The evidence source is malformed.")))))

(-> knowledge-replace-observation
    (configuration list list &key (:scope memory-scope) (:source-conversation (option string)))
    string)
(defun knowledge-replace-observation (configuration proposition evidence
                                      &key (scope ':workspace) source-conversation)
  "Record that EVIDENCE observed PROPOSITION, replacing that source's earlier report.

Supports of claims with the same schema key whose evidence came from the same
source are retracted in the same transaction, so an adapter reporting a
mission's new status does not leave its old status standing. Return the claim."
  (let* ((workspace (knowledge--owner configuration scope))
         (claim (knowledge-claim-identifier proposition :scope scope :workspace workspace))
         (source-identity (knowledge--source-identity (getf (rest evidence) :source))))
    (knowledge-transact
     configuration
     (lambda (base)
       (let* ((schema (or (knowledge-base-schema base (knowledge-proposition-predicate proposition)
                                                 :workspace (knowledge--workspace configuration))
                          (knowledge--fail ':unknown-predicate
                                           (knowledge-proposition-predicate proposition)
                                           "Predicate ~A is not defined."
                                           (knowledge-proposition-predicate proposition))))
              (key (knowledge-proposition-key proposition schema))
              (time (get-universal-time))
              (superseded
                (loop for other in (knowledge-base-claims-with-predicate
                                    base (knowledge-proposition-predicate proposition))
                      for entry = (knowledge-base-claim base other)
                      when (and (string/= other claim)
                                (eq (getf entry :scope) scope)
                                (equal (getf entry :workspace) workspace)
                                key
                                (equal (knowledge-proposition-key (getf entry :proposition) schema)
                                       key))
                        append (loop for support-identifier in (getf entry :supports)
                                     for support = (knowledge-base-support base support-identifier)
                                     for cited = (and (getf support :evidence)
                                                      (knowledge-base-evidence-record
                                                       base (getf support :evidence)))
                                     when (and (null (getf support :retracted-at))
                                               cited
                                               (equal (knowledge--source-identity
                                                       (getf cited :source))
                                                      source-identity))
                                       collect support-identifier)))
              (active-p
                (some (lambda (support-identifier)
                        (let ((support (knowledge-base-support base support-identifier)))
                          (and (null (getf support :retracted-at))
                               (eq (getf support :basis) ':observation)
                               (null (getf support :valid-from))
                               (null (getf support :valid-until))
                               (equal (getf support :evidence) (getf (rest evidence) :id)))))
                      (getf (knowledge-base-claim base claim) :supports)))
               (observation-records (knowledge--observation-records base evidence)))
         (unless (knowledge-proposition-valid-p proposition schema)
           (knowledge--fail ':invalid-claim (knowledge-proposition-predicate proposition)
                            "The observation does not fit the schema of ~A."
                            (knowledge-proposition-predicate proposition)))
         (values
          (append
           (mapcar (lambda (support)
                     (list :support-retracted
                           :version *knowledge-format-version*
                           :support support
                           :reason "superseded by a newer observation from the same source"
                           :time time
                           :source-conversation source-conversation))
                   superseded)
            observation-records
           (unless active-p
             (list (list :claim-asserted
                         :version *knowledge-format-version*
                         :support (make-identifier)
                         :claim claim
                         :scope scope
                         :workspace workspace
                         :proposition proposition
                         :evidence (getf (rest evidence) :id)
                         :basis ':observation
                         :valid-from nil
                         :valid-until nil
                         :time time
                         :source-conversation source-conversation))))
          claim
          (or superseded observation-records (not active-p))))))))
