(in-package #:autolith)

;;;; -- Persistent Papercuts --

(defparameter *papercut-format-version* 1
  "The readable persistent papercut format version.")

(defparameter *papercut-title-limit* 200
  "The maximum characters in one papercut title.")

(defparameter *papercut-content-limit* 8000
  "The maximum characters in one papercut report.")

(defparameter *papercut-resolution-limit* 1000
  "The maximum characters in one papercut closure resolution.")

(defparameter *papercut-assessment-note-limit* 1000
  "The maximum characters in one papercut effectiveness assessment note.")

(defparameter *papercut-short-identifier-length* 8
  "The characters shown when a papercut identifier is abbreviated.")

(defparameter *papercut-duplicate-window-seconds* 86400
  "How long an active report absorbs new reports of the same problem.")

(defparameter *papercut-duplicate-similarity* 3/5
  "The word-set similarity at which a new report repeats an active one.")

(defparameter *papercut-duplicate-session-window-seconds* 3600
  "How long a report from the same conversation is judged by the looser session similarity.")

(defparameter *papercut-duplicate-session-similarity* 2/5
  "The word-set similarity at which a report repeats one its own conversation filed recently.

A conversation that keeps re-filing a problem rewrites the report each time,
so its repeats share fewer words than reports from different sessions do.")

(defparameter *papercut-clock* #'get-universal-time
  "The function returning the universal time stamped on new reports.")

(defvar *papercut-lock* (make-lock "Autolith persistent papercuts")
  "The process-local lock serializing papercut reads and appends.")

(defclass papercut ()
  ((identifier
    :initarg :identifier
    :reader papercut-identifier
    :type non-empty-string
    :documentation "The stable identifier of this papercut report.")
   (reported-at
    :initarg :reported-at
    :reader papercut-reported-at
    :type timestamp
    :documentation "The time at which the report was recorded.")
   (workspace
    :initarg :workspace
    :reader papercut-workspace
    :type non-empty-string
    :documentation "The workspace in which the problem was observed.")
   (title
    :initarg :title
    :reader papercut-title
    :type non-empty-string
    :documentation "A short description of the problem.")
   (content
    :initarg :content
    :reader papercut-content
    :type non-empty-string
    :documentation "The complete user-visible problem report.")
   (issue-kind
    :initarg :issue-kind :initform "other" :reader papercut-issue-kind
    :type string :documentation "The explicit structured category of the observed problem.")
   (tool
    :initarg :tool :initform "unknown" :reader papercut-tool
    :type string :documentation "The affected public tool category, or unknown.")
    (assessment-verdict
     :initform nil
     :accessor papercut-assessment-verdict
     :type (option (member :improved :worse :unchanged :too-early))
     :documentation "The verdict from the latest effectiveness assessment, if any.")
    (assessment-note
     :initform nil
     :accessor papercut-assessment-note
     :type (option string)
     :documentation "The bounded explanatory note from the latest assessment, if any.")
    (assessed-at
     :initform nil
     :accessor papercut-assessed-at
     :type (option timestamp)
     :documentation "The time of the latest effectiveness assessment, if any.")
   (source-conversation
    :initarg :source-conversation
    :reader papercut-source-conversation
    :type (option string)
    :documentation "The conversation that most recently reported the papercut."))
  (:documentation "One persistent user-visible report of an Autolith problem."))


;;;; -- Validation and Records --

(-> papercut--validate-text (t string integer) string)
(defun papercut--validate-text (value field limit)
  "Return non-empty string VALUE after validating FIELD and LIMIT."
  (unless (non-empty-string-p value)
    (error 'papercut-error
           :message (format nil "Papercut ~A must be a non-empty string." field)
           :pathname #P"papercuts.sexp"
           :identifier nil))
  (when (> (length value) limit)
    (error 'papercut-error
           :message (format nil "Papercut ~A exceeds the ~:D-character limit."
                            field
                            limit)
           :pathname #P"papercuts.sexp"
           :identifier nil))
  value)

(-> papercut--record (papercut) list)
(defun papercut--record (papercut)
  "Return the complete portable record for PAPERCUT."
  (list :papercut
        :version *papercut-format-version*
        :id (papercut-identifier papercut)
        :reported-at (papercut-reported-at papercut)
        :workspace (papercut-workspace papercut)
        :title (papercut-title papercut)
        :content (papercut-content papercut)
        :issue-kind (papercut-issue-kind papercut)
        :tool (papercut-tool papercut)
        :source-conversation (papercut-source-conversation papercut)))

(-> papercut--issue-kind (t) string)
(defun papercut--issue-kind (value)
  "Validate an explicit issue category without inferring it from private prose."
  (unless (member value '("broken_tool" "misleading_success" "authorization" "performance" "other")
                  :test #'equal)
    (error 'papercut-error :message "Unknown papercut issue category."
                          :pathname #P"papercuts.sexp" :identifier nil))
  value)

(-> papercut--note-report (papercut boolean) null)
(defun papercut--note-report (papercut duplicate-p)
  "Export a newly committed report through the consent-gated projection."
  (unless duplicate-p
    (telemetry--call-safely
     (lambda ()
       (telemetry-note-report :tool (papercut-tool papercut)
                             :issue-kind (papercut-issue-kind papercut)
                             :summary-function (lambda () (papercut-title papercut))))))
  nil)

(-> papercut--note-repair (papercut string string) null)
(defun papercut--note-repair (papercut outcome summary)
  "Export a committed assessment or closure without claiming automatic verification."
  (telemetry--call-safely
   (lambda ()
     (telemetry-note-repair :target (papercut-tool papercut) :repair-kind "other"
                           :outcome outcome :verified nil
                           :summary-function (lambda () summary))))
  nil)

(-> papercut--closed-record (string string timestamp) list)
(defun papercut--closed-record (identifier resolution closed-at)
  "Return one portable closure record for IDENTIFIER and RESOLUTION."
  (list :papercut-closed
        :version *papercut-format-version*
        :id identifier
        :closed-at closed-at
        :resolution resolution))

(-> papercut--assessed-record
    (string (member :improved :worse :unchanged :too-early) string timestamp)
    list)
(defun papercut--assessed-record (identifier verdict note assessed-at)
  "Return one portable effectiveness assessment record for IDENTIFIER."
  (list :papercut-assessed
        :version *papercut-format-version*
        :id identifier
        :assessed-at assessed-at
        :verdict verdict
        :note note))

(-> papercut--validate-assessed-record (pathname list) list)
(defun papercut--validate-assessed-record (pathname record)
  "Validate assessment RECORD from PATHNAME and return its replay values."
  (let ((version (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (assessed-at (getf (rest record) :assessed-at))
        (verdict (getf (rest record) :verdict))
        (note (getf (rest record) :note)))
    (unless (and (eql version *papercut-format-version*)
                 (non-empty-string-p identifier)
                 (typep assessed-at 'timestamp)
                 (member verdict '(:improved :worse :unchanged :too-early)))
      (error 'papercut-error
             :message "A papercut assessment record has invalid metadata."
             :pathname pathname
             :identifier (and (stringp identifier) identifier)))
    (handler-case
        (papercut--validate-text
         note "assessment note" *papercut-assessment-note-limit*)
      (papercut-error (condition)
        (error 'papercut-error
               :message (autolith-error-message condition)
               :pathname pathname
               :identifier identifier)))
    (list identifier verdict note assessed-at)))

(-> papercut--validate-closed-record (pathname list) string)
(defun papercut--validate-closed-record (pathname record)
  "Validate closure RECORD from PATHNAME and return its identifier."
  (let ((version (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (closed-at (getf (rest record) :closed-at))
        (resolution (getf (rest record) :resolution)))
    (unless (and (eql version *papercut-format-version*)
                 (non-empty-string-p identifier)
                 (typep closed-at 'timestamp))
      (error 'papercut-error
             :message "A papercut closure record has invalid metadata."
             :pathname pathname
             :identifier (and (stringp identifier) identifier)))
    (handler-case
        (papercut--validate-text
         resolution "closure resolution" *papercut-resolution-limit*)
      (papercut-error (condition)
        (error 'papercut-error
               :message (autolith-error-message condition)
               :pathname pathname
               :identifier identifier)))
    identifier))

(-> papercut--record->papercut (pathname list) papercut)
(defun papercut--record->papercut (pathname record)
  "Validate and convert one portable papercut RECORD from PATHNAME."
  (let ((version (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (reported-at (getf (rest record) :reported-at))
        (workspace (getf (rest record) :workspace))
        (title (getf (rest record) :title))
        (content (getf (rest record) :content))
        (source-conversation (getf (rest record) :source-conversation)))
    (unless (and (eql version *papercut-format-version*)
                 (non-empty-string-p identifier)
                 (typep reported-at 'timestamp)
                 (non-empty-string-p workspace)
                 (or (null source-conversation)
                     (non-empty-string-p source-conversation)))
      (error 'papercut-error
             :message "A persistent papercut record has invalid metadata."
             :pathname pathname
             :identifier (and (stringp identifier) identifier)))
    (handler-case
        (make-instance 'papercut
                       :identifier identifier
                       :reported-at reported-at
                       :workspace workspace
                       :title (papercut--validate-text
                               title "title" *papercut-title-limit*)
                       :content (papercut--validate-text
                                 content "content" *papercut-content-limit*)
                       :issue-kind (papercut--issue-kind (getf (rest record) :issue-kind "other"))
                       :tool (telemetry--tool (getf (rest record) :tool))
                       :source-conversation source-conversation)
      (papercut-error (condition)
        (error 'papercut-error
               :message (autolith-error-message condition)
               :pathname pathname
               :identifier identifier)))))


;;;; -- Readable Log --

(-> papercut--store (configuration) sexp-store:log-store)
(defun papercut--store (configuration)
  "Describe papercut paths and lifecycle validation for the transaction store."
  (let ((pathname (configuration-papercut-path configuration)))
    (make-instance
     'sexp-store:log-store
     :pathname pathname
     :lock-pathname (readable-state-lock-pathname pathname "papercuts.lock")
     :header (list :papercuts :version *papercut-format-version*)
     :header-validator
     (lambda (form)
       (sexp-store:record-check form :tag ':papercuts
                               :versions (list *papercut-format-version*)))
     :validator (lambda (form) (keywordp (first form)))
     :initial-state (lambda () (cons (make-hash-table :test #'equal)
                                     (make-hash-table :test #'equal)))
     :reducer (lambda (state record)
                (papercut--reduce-record pathname state record))
     :finalizer #'papercut--sort-active)))

(-> papercut--reduce-record (pathname cons list) cons)
(defun papercut--reduce-record (pathname state record)
  "Apply one report, assessment, or closure to private lifecycle STATE."
  (let ((seen (first state))
        (active (rest state)))
    (case (first record)
      (:papercut
       (let* ((papercut (papercut--record->papercut pathname record))
              (identifier (papercut-identifier papercut)))
         (when (gethash identifier seen)
           (error 'papercut-error
                  :message (format nil
                                   "Persistent papercut identifier ~A occurs more than once."
                                   identifier)
                  :pathname pathname
                  :identifier identifier))
         (setf (gethash identifier seen) t
               (gethash identifier active) papercut)))
      (:papercut-assessed
       (destructuring-bind (identifier verdict note assessed-at)
           (papercut--validate-assessed-record pathname record)
         (let ((papercut (gethash identifier active)))
           (unless papercut
             (error 'papercut-error
                    :message (format nil
                                     "Papercut assessment references inactive or unknown identifier ~A."
                                     identifier)
                    :pathname pathname
                    :identifier identifier))
           (setf (papercut-assessment-verdict papercut) verdict
                 (papercut-assessment-note papercut) note
                 (papercut-assessed-at papercut) assessed-at))))
      (:papercut-closed
       (let ((identifier (papercut--validate-closed-record pathname record)))
         (unless (gethash identifier seen)
           (error 'papercut-error
                  :message (format nil "Papercut closure references unknown identifier ~A."
                                   identifier)
                  :pathname pathname
                  :identifier identifier))
         (unless (gethash identifier active)
           (error 'papercut-error
                  :message (format nil "Papercut identifier ~A is closed more than once."
                                   identifier)
                  :pathname pathname
                  :identifier identifier))
         (when (member (papercut-assessment-verdict (gethash identifier active))
                       '(:worse :unchanged :too-early))
           (error 'papercut-error
                  :message (format nil
                                   "Papercut ~A closure conflicts with its latest ~A assessment."
                                   identifier
                                   (papercut-assessment-verdict (gethash identifier active)))
                  :pathname pathname
                  :identifier identifier))
         (remhash identifier active)))
      (otherwise
       (error 'papercut-error
              :message (format nil "Unsupported persistent papercut record ~S."
                               (first record))
              :pathname pathname
              :identifier nil)))
    state))

(-> papercut--sort-active (cons) list)
(defun papercut--sort-active (state)
  "Return active reports in deterministic newest-first presentation order."
  (sort (loop for papercut being the hash-values of (rest state) collect papercut)
        (lambda (left right)
          (or (> (papercut-reported-at left) (papercut-reported-at right))
              (and (= (papercut-reported-at left) (papercut-reported-at right))
                   (string< (papercut-identifier left)
                            (papercut-identifier right)))))))

(-> papercut--transact (configuration function) t)
(defun papercut--transact (configuration update)
  "Apply UPDATE through the store, translating storage failures for callers."
  (handler-case
      (sexp-store:store-transact (papercut--store configuration) update)
    (sexp-store:store-error (cause)
      (error 'papercut-error
             :message (sexp-store:store-error-message cause)
             :pathname (sexp-store:store-error-pathname cause)
             :identifier nil))))


;;;; -- Selection and Mutation --

(-> papercut--workspace (configuration) non-empty-string)
(defun papercut--workspace (configuration)
  "Return CONFIGURATION's current workspace identity used by papercut records."
  (namestring (config :working-directory configuration)))

(-> papercut--workspace-reports (configuration list) list)
(defun papercut--workspace-reports (configuration active)
  "Select current-workspace reports from ACTIVE transaction state."
  (let ((workspace (papercut--workspace configuration)))
    (remove-if-not
     (lambda (papercut)
       (string= workspace (papercut-workspace papercut)))
     active)))

(-> papercut-list (configuration) list)
(defun papercut-list (configuration)
  "Return papercuts reported in CONFIGURATION's current workspace, newest first."
  (with-lock-held (*papercut-lock*)
    (papercut--transact
     configuration
     (lambda (active)
       (values nil (papercut--workspace-reports configuration active) nil)))))

(-> papercut-find (configuration string) (option papercut))
(defun papercut-find (configuration identifier)
  "Return the exact active papercut IDENTIFIER in CONFIGURATION's workspace."
  (find identifier
        (papercut-list configuration)
        :test #'string=
        :key #'papercut-identifier))

(-> papercut-resolve
    (configuration string)
    (values (option papercut) (member :missing :ambiguous :found) list))
(defun papercut-resolve (configuration identifier)
  "Resolve an exact or unique identifier prefix in the current workspace.

The second value is :FOUND, :MISSING, or :AMBIGUOUS. The third value contains
matching reports for :AMBIGUOUS."
  (if (non-empty-string-p identifier)
      (let* ((papercuts (papercut-list configuration))
             (matches (remove-if-not
                       (lambda (papercut)
                         (uiop:string-prefix-p
                          identifier
                          (papercut-identifier papercut)))
                       papercuts)))
        (cond
          ((null matches)
           (values nil ':missing nil))
          ((null (rest matches))
           (values (first matches) ':found nil))
          (t
           (values nil ':ambiguous matches))))
      (values nil ':missing nil)))

(-> papercut--tokens (string) list)
(defun papercut--tokens (text)
  "Return TEXT's distinct lowercase words of three or more characters.

Words keep inner dots, slashes, colons, hyphens, and underscores, so tool and
symbol names such as lisp.source survive, while surrounding punctuation drops."
  (let ((tokens nil)
        (start nil))
    (flet ((flush (end)
             (when start
               (let ((word (string-trim "-_./:"
                                        (string-downcase (subseq text start end)))))
                 (when (>= (length word) 3)
                   (pushnew word tokens :test #'string=)))
               (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (or (alphanumericp character)
                       (find character "-_./:"))
                   (unless start
                     (setf start index))
                   (flush index))
            finally (flush (length text))))
    tokens))

(-> papercut--similarity (list list) rational)
(defun papercut--similarity (left right)
  "Return the Jaccard similarity of word lists LEFT and RIGHT, from 0 to 1."
  (let ((union (union left right :test #'string=)))
    (if (null union)
        0
        (/ (length (intersection left right :test #'string=))
           (length union)))))

(-> papercut--same-words-p (list list) boolean)
(defun papercut--same-words-p (left right)
  "Return true when word lists LEFT and RIGHT hold the same words."
  (and (subsetp left right :test #'string=)
       (subsetp right left :test #'string=)))

(-> papercut--duplicate
    (configuration list non-empty-string non-empty-string (option string) integer)
    (option papercut))
(defun papercut--duplicate (configuration active title content source-conversation now)
  "Return the recent active report of this workspace that TITLE and CONTENT repeat.

A report repeats another filed within *PAPERCUT-DUPLICATE-WINDOW-SECONDS* when
their titles hold the same words, or when the words of title and body together
overlap at *PAPERCUT-DUPLICATE-SIMILARITY* or more. A report from
SOURCE-CONVERSATION filed within *PAPERCUT-DUPLICATE-SESSION-WINDOW-SECONDS*
only needs *PAPERCUT-DUPLICATE-SESSION-SIMILARITY*. The closest match wins."
  (let ((title-words (papercut--tokens title))
        (words (union (papercut--tokens title) (papercut--tokens content)
                      :test #'string=))
        (best nil)
        (best-score 0))
    (dolist (papercut (papercut--workspace-reports configuration active) best)
      (let ((age (abs (- now (papercut-reported-at papercut)))))
        (when (<= age *papercut-duplicate-window-seconds*)
          (let* ((other-title-words (papercut--tokens (papercut-title papercut)))
                 (other-words (union other-title-words
                                     (papercut--tokens (papercut-content papercut))
                                     :test #'string=))
                 (score (if (papercut--same-words-p title-words other-title-words)
                            1
                            (papercut--similarity words other-words)))
                 (threshold
                   (if (and source-conversation
                            (equal source-conversation
                                   (papercut-source-conversation papercut))
                            (<= age *papercut-duplicate-session-window-seconds*))
                       *papercut-duplicate-session-similarity*
                       *papercut-duplicate-similarity*)))
            (when (and (>= score threshold)
                       (> score best-score))
              (setf best papercut
                    best-score score))))))))

(-> papercut--report-unlocked
    (configuration &key (:title non-empty-string) (:content non-empty-string)
                        (:source-conversation (option string))
                        (:issue-kind string) (:tool (option string))
                        (:active list))
    (values list list boolean))
(defun papercut--report-unlocked
    (configuration &key title content source-conversation (issue-kind "other") tool active)
  "Return the records to append and (PAPERCUT DUPLICATE-P) for a report.

When the report repeats an ACTIVE papercut, nothing is appended and that
existing papercut is returned with DUPLICATE-P true."
  (let* ((now (funcall *papercut-clock*))
         (duplicate (papercut--duplicate configuration
                                         (remove-if-not
                                          (lambda (item)
                                            (and (equal (papercut-issue-kind item) issue-kind)
                                                 (equal (papercut-tool item) (telemetry--tool tool))))
                                          active) title content
                                         source-conversation now)))
    (if duplicate
        (values nil (list duplicate t) nil)
        (let ((papercut
                (make-instance
                 'papercut
                 :identifier (make-identifier)
                 :reported-at now
                 :workspace (papercut--workspace configuration)
                 :title title
                 :content content
                 :issue-kind (papercut--issue-kind issue-kind)
                 :tool (telemetry--tool tool)
                 :source-conversation source-conversation)))
          (values (list (papercut--record papercut)) (list papercut nil) t)))))

(-> papercut-report
    (configuration &key (:title string) (:content string)
                   (:issue-kind string) (:tool (option string))
                   (:source-conversation (option string)))
    (values papercut boolean))
(defun papercut-report (configuration &key title content source-conversation (issue-kind "other") tool)
  "Record one new user-visible report about a problem in the current workspace.

Return the recorded papercut and NIL, or, when the report repeats an active
papercut of the workspace, that existing papercut and T without recording."
  (let ((validated-title
          (papercut--validate-text title "title" *papercut-title-limit*))
        (validated-content
          (papercut--validate-text content "content" *papercut-content-limit*)))
    (unless (or (null source-conversation)
                (non-empty-string-p source-conversation))
      (error 'papercut-error
             :message "Papercut source conversation must be a non-empty string."
             :pathname (configuration-papercut-path configuration)
             :identifier nil))
    (destructuring-bind (papercut duplicate-p)
        (with-lock-held (*papercut-lock*)
          (papercut--transact
           configuration
           (lambda (active)
             (papercut--report-unlocked
              configuration :title validated-title :content validated-content
                            :issue-kind issue-kind :tool tool
                            :source-conversation source-conversation
                            :active active))))
      (papercut--note-report papercut duplicate-p)
      (values papercut duplicate-p))))

(-> papercut--assess-unlocked
    (configuration non-empty-string
     &key (:active list) (:verdict (member :improved :worse :unchanged :too-early))
          (:note non-empty-string))
    (values list papercut boolean))
(defun papercut--assess-unlocked (configuration identifier &key active verdict note)
  "Return the assessment event and its updated report from private state."
  (let ((papercut
          (find identifier
                (papercut--workspace-reports configuration active)
                :test #'string=
                :key #'papercut-identifier))
        (assessed-at (get-universal-time)))
    (unless papercut
      (error 'papercut-error
             :message (format nil
                              "No active papercut ~A exists in this workspace."
                              identifier)
             :pathname (configuration-papercut-path configuration)
             :identifier identifier))
    (setf (papercut-assessment-verdict papercut) verdict
          (papercut-assessment-note papercut) note
          (papercut-assessed-at papercut) assessed-at)
    (values
     (list (papercut--assessed-record identifier verdict note assessed-at))
     papercut t)))

(-> papercut-assess
    (configuration string &key (:verdict keyword) (:note string))
    papercut)
(defun papercut-assess (configuration identifier &key verdict note)
  "Append an effectiveness assessment for active papercut IDENTIFIER."
  (unless (non-empty-string-p identifier)
    (error 'papercut-error
           :message "Papercut identifier must be a non-empty string."
           :pathname (configuration-papercut-path configuration)
           :identifier nil))
  (unless (member verdict '(:improved :worse :unchanged :too-early))
    (error 'papercut-error
           :message "Papercut assessment verdict must be improved, worse, unchanged, or too-early."
           :pathname (configuration-papercut-path configuration)
           :identifier identifier))
  (let ((validated-note
          (papercut--validate-text
           note "assessment note" *papercut-assessment-note-limit*)))
    (let ((papercut
            (with-lock-held (*papercut-lock*)
              (papercut--transact
               configuration
               (lambda (active)
                 (papercut--assess-unlocked
                  configuration identifier :active active :verdict verdict
                                           :note validated-note))))))
      (papercut--note-repair
       papercut (case verdict
                  (:improved "applied")
                  (:worse "failed")
                  (otherwise "proposed"))
       validated-note)
      papercut)))

(-> papercut--mark-closed-unlocked
    (configuration non-empty-string non-empty-string &key (:active list))
    (values list papercut boolean))
(defun papercut--mark-closed-unlocked
    (configuration identifier resolution &key active)
  "Return a closure event after checking the workspace and assessment policy."
  (let* ((workspace (papercut--workspace configuration))
         (papercut
           (find-if
            (lambda (candidate)
              (and (string= identifier (papercut-identifier candidate))
                   (string= workspace (papercut-workspace candidate))))
            active)))
    (unless papercut
      (error 'papercut-error
             :message (format nil
                              "No active papercut ~A exists in this workspace."
                              identifier)
             :pathname (configuration-papercut-path configuration)
             :identifier identifier))
    (when (member (papercut-assessment-verdict papercut)
                  '(:worse :unchanged :too-early))
      (error 'papercut-error
             :message (format nil
                              "Papercut ~A cannot be closed while its latest assessment is ~A."
                              identifier
                              (papercut-assessment-verdict papercut))
             :pathname (configuration-papercut-path configuration)
             :identifier identifier))
    (values
     (list (papercut--closed-record identifier resolution (get-universal-time)))
     papercut t)))

(-> papercut-mark-closed (configuration string &key (:resolution string)) papercut)
(defun papercut-mark-closed (configuration identifier &key resolution)
  "Close active papercut IDENTIFIER with a durable RESOLUTION and return it."
  (unless (non-empty-string-p identifier)
    (error 'papercut-error
           :message "Papercut identifier must be a non-empty string."
           :pathname (configuration-papercut-path configuration)
           :identifier nil))
  (let ((validated-resolution
          (papercut--validate-text
           resolution "closure resolution" *papercut-resolution-limit*)))
    (let ((papercut
            (with-lock-held (*papercut-lock*)
              (papercut--transact
               configuration
               (lambda (active)
                 (papercut--mark-closed-unlocked
                  configuration identifier validated-resolution :active active))))))
      (papercut--note-repair papercut "applied" validated-resolution)
      papercut)))


;;;; -- Presentation Values --

(-> papercut-timestamp-string (timestamp) string)
(defun papercut-timestamp-string (timestamp)
  "Return TIMESTAMP as an ISO-8601 UTC string."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

(-> papercut-short-identifier (papercut) string)
(defun papercut-short-identifier (papercut)
  "Return the stable abbreviated identifier shown in the terminal."
  (subseq (papercut-identifier papercut)
          0
          (min *papercut-short-identifier-length*
               (length (papercut-identifier papercut)))))

(-> papercut-call-source (papercut) string)
(defun papercut-call-source (papercut)
  "Return the canonical Lisp call that opens PAPERCUT."
  (format nil "(papercut ~S)" (papercut-short-identifier papercut)))

(-> papercut-excerpt (string integer) string)
(defun papercut-excerpt (content limit)
  "Return a single-line prefix of CONTENT no longer than LIMIT characters."
  (let* ((single-line
           (with-output-to-string (stream)
             (loop with spacing-p = nil
                   for character across content
                   if (find character '(#\Space #\Tab #\Newline #\Return))
                     do (unless spacing-p
                          (write-char #\Space stream)
                          (setf spacing-p t))
                   else
                     do (write-char character stream)
                        (setf spacing-p nil))))
         (trimmed (string-trim '(#\Space) single-line)))
    (if (<= (length trimmed) limit)
        trimmed
        (format nil "~A..." (subseq trimmed 0 (max 0 (- limit 3)))))))
