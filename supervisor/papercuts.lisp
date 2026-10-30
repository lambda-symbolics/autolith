
(in-package #:autolith-supervisor)

(defgeneric papercut-context-path
    (context)
  (:documentation "Return the trusted private papercut log pathname."))

(defgeneric papercut-context-workspace
    (context)
  (:documentation "Return the trusted workspace identity."))

(defgeneric papercut-context-now
    (context)
  (:documentation "Return a trusted universal time for a lifecycle event."))

(defgeneric papercut-context-identifier
    (context)
  (:documentation "Return a fresh stable report identifier."))

(defclass papercut-context nil
          ((pathname :initarg :pathname :reader papercut-context-path :type
                     pathname :documentation
                     "The trusted private log pathname.")
           (workspace :initarg :workspace :reader papercut-context-workspace
            :type string :documentation "The trusted workspace identity.")
           (clock :initarg :clock :initform #'get-universal-time :reader
            papercut-context-clock :documentation
            "The trusted universal-time source.")
           (identifier-source :initarg :identifier-source :initform
            #'papercut--make-identifier :reader
            papercut-context-identifier-source :documentation
            "The trusted random identifier source."))
          (:documentation
           "Explicit authority for one private papercut log and workspace."))

(defmethod papercut-context-now ((context papercut-context))
  "Read and validate the trusted lifecycle clock."
  (let ((now (funcall (papercut-context-clock context))))
    (check-type now (integer 0))
    now))

(defmethod papercut-context-identifier ((context papercut-context))
  "Allocate a stable identifier through trusted authority."
  (papercut--validate-text
   (funcall (papercut-context-identifier-source context)) "identifier" 1024))

(serapeum/types:-> papercut--make-identifier nil string)
(defun papercut--make-identifier ()
  "Return an operating-system random identifier."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(define-condition papercut-error
    (autolith-error)
    ((pathname :initarg :pathname :reader papercut-error-pathname :type
               pathname :documentation "The affected private store pathname.")
     (identifier :initarg :identifier :reader papercut-error-identifier
      :documentation "The affected report identity, when known."))
  (:documentation "Papercut validation or persistence failed."))

(serapeum/types:-> papercut--string-p (t) boolean)
(defun papercut--string-p (value)
  "Return whether VALUE is a nonempty string."
  (and (stringp value) (plusp (length value))))

(deftype papercut-string ()
  "A nonempty papercut string."
  '(and string (satisfies papercut--string-p)))

(serapeum/types:-> papercut--diagnostic (t) (or null string))
(defun papercut--diagnostic (value)
  "Validate optional explicitly supplied diagnostic text."
  (when value (papercut--validate-text value "diagnostic" 8000)))

(serapeum/types:-> papercut--tool (t) (or null string))
(defun papercut--tool (value)
  "Validate an optional public tool name without inferring it from prose."
  (when value (papercut--validate-text value "tool" 200)))

(serapeum/types:-> papercut--export-id-p (t) boolean)
(defun papercut--export-id-p (value)
  "Accept a nonzero 128-bit hexadecimal export identity from an existing log."
  (and (stringp value) (= (length value) 32)
       (every (lambda (character) (digit-char-p character 16)) value)
       (not (every (lambda (character) (char= character #\0)) value)) t))

(deftype papercut-time () "A universal lifecycle timestamp." '(integer 0))

(defparameter *papercut-format-version*
  1
  "The readable persistent papercut format version.")

(defparameter *papercut-title-limit*
  200
  "The maximum characters in one papercut title.")

(defparameter *papercut-content-limit*
  8000
  "The maximum characters in one papercut report.")

(defparameter *papercut-resolution-limit*
  1000
  "The maximum characters in one papercut closure resolution.")

(defparameter *papercut-assessment-note-limit*
  1000
  "The maximum characters in one papercut effectiveness assessment note.")

(defparameter *papercut-short-identifier-length*
  8
  "The characters shown when a papercut identifier is abbreviated.")

(defparameter *papercut-duplicate-window-seconds*
  86400
  "How long an active report absorbs new reports of the same problem.")

(defparameter *papercut-duplicate-similarity*
  3/5
  "The word-set similarity at which a new report repeats an active one.")

(defparameter *papercut-duplicate-session-window-seconds*
  3600
  "How long a report from the same conversation is judged by the looser session similarity.")

(defparameter *papercut-duplicate-session-similarity*
  2/5
  "The word-set similarity at which a report repeats one its own conversation filed recently.

A conversation that keeps re-filing a problem rewrites the report each time,
so its repeats share fewer words than reports from different sessions do.")

(defvar *papercut-lock*
  (bordeaux-threads:make-lock "Autolith persistent papercuts")
  "The process-local lock serializing papercut reads and appends.")

(defclass papercut nil
          ((identifier :initarg :identifier :reader papercut-identifier :type
            papercut-string :documentation
            "The stable identifier of this papercut report.")
           (reported-at :initarg :reported-at :reader papercut-reported-at
            :type papercut-time :documentation
            "The time at which the report was recorded.")
           (workspace :initarg :workspace :reader papercut-workspace :type
            papercut-string :documentation
            "The workspace in which the problem was observed.")
           (title :initarg :title :reader papercut-title :type papercut-string
            :documentation "A short description of the problem.")
           (content :initarg :content :reader papercut-content :type
            papercut-string :documentation
            "The complete user-visible problem report.")
           (issue-kind :initarg :issue-kind :initform "other" :reader
            papercut-issue-kind :type string :documentation
            "The explicit structured category of the observed problem.")
           (diagnostic :initarg :diagnostic :initform nil :reader
            papercut-diagnostic :type (or null string) :documentation
            "An explicit explanation intended for optional redacted telemetry.")
           (tool :initarg :tool :initform nil :reader papercut-tool :type
            (or null string) :documentation
            "The affected public tool category, when known.")
           (telemetry-id :initarg :telemetry-id :initform nil :accessor
            papercut-telemetry-id :type (or null string) :documentation
            "An independently random issue export ID, allocated only with consent.")
           (assessment-verdict :initform nil :accessor
            papercut-assessment-verdict :type
            (or null (member :improved :worse :unchanged :too-early))
            :documentation
            "The verdict from the latest effectiveness assessment, if any.")
           (assessment-note :initform nil :accessor papercut-assessment-note
            :type (or null string) :documentation
            "The bounded explanatory note from the latest assessment, if any.")
           (assessed-at :initform nil :accessor papercut-assessed-at :type
            (or null papercut-time) :documentation
            "The time of the latest effectiveness assessment, if any.")
           (source-conversation :initarg :source-conversation :reader
            papercut-source-conversation :type (or null string) :documentation
            "The conversation that most recently reported the papercut."))
          (:documentation
           "One persistent user-visible report of an Autolith problem."))

(serapeum/types:-> papercut--validate-text (t string integer) string)
(defun papercut--validate-text (value field limit)
  "Return non-empty string VALUE after validating FIELD and LIMIT."
  (unless (papercut--string-p value)
    (error 'papercut-error :message
           (format nil "Papercut ~A must be a non-empty string." field)
           :pathname #P"papercuts.sexp" :identifier nil))
  (when (> (length value) limit)
    (error 'papercut-error :message
           (format nil "Papercut ~A exceeds the ~:D-character limit." field
                   limit)
           :pathname #P"papercuts.sexp" :identifier nil))
  value)

(serapeum/types:-> papercut--record (papercut) list)
(defun papercut--record (papercut)
  "Return the complete portable record for PAPERCUT."
  (list :papercut :version *papercut-format-version* :id
        (papercut-identifier papercut) :reported-at
        (papercut-reported-at papercut) :workspace
        (papercut-workspace papercut) :title (papercut-title papercut) :content
        (papercut-content papercut) :diagnostic (papercut-diagnostic papercut)
        :issue-kind (papercut-issue-kind papercut) :tool
        (papercut-tool papercut) :telemetry-id (papercut-telemetry-id papercut)
        :source-conversation (papercut-source-conversation papercut)))

(serapeum/types:-> papercut--issue-kind (t) string)
(defun papercut--issue-kind (value)
  "Validate an explicit issue category without inferring it from private prose."
  (unless
      (member value
              '("broken_tool" "misleading_success" "authorization"
                "performance" "other")
              :test #'equal)
    (error 'papercut-error :message "Unknown papercut issue category."
           :pathname #P"papercuts.sexp" :identifier nil))
  value)

(serapeum/types:-> papercut--closed-record
                   (string string papercut-time &key
                           (:diagnostic (or null string)))
                   list)
(defun papercut--closed-record
       (identifier resolution closed-at &key diagnostic)
  "Return one portable closure record for IDENTIFIER and RESOLUTION."
  (list :papercut-closed :version *papercut-format-version* :id identifier
        :closed-at closed-at :resolution resolution :diagnostic diagnostic))

(serapeum/types:-> papercut--assessed-record
                   (string &key
                           (:verdict
                            (member :improved :worse :unchanged :too-early))
                           (:note string) (:assessed-at papercut-time)
                           (:diagnostic (or null string)))
                   list)
(defun papercut--assessed-record
       (identifier &key verdict note assessed-at diagnostic)
  "Return one portable effectiveness assessment record for IDENTIFIER."
  (list :papercut-assessed :version *papercut-format-version* :id identifier
        :assessed-at assessed-at :verdict verdict :note note :diagnostic
        diagnostic))

(serapeum/types:-> papercut--validate-assessed-record (pathname list) list)
(defun papercut--validate-assessed-record (pathname record)
  "Validate assessment RECORD from PATHNAME and return its replay values."
  (let ((version     (getf (rest record) :version))
        (identifier  (getf (rest record) :id))
        (assessed-at (getf (rest record) :assessed-at))
        (verdict     (getf (rest record) :verdict))
        (note        (getf (rest record) :note)))
    (unless
        (and (eql version *papercut-format-version*)
             (papercut--string-p identifier) (typep assessed-at 'papercut-time)
             (member verdict '(:improved :worse :unchanged :too-early)))
      (error 'papercut-error :message
             "A papercut assessment record has invalid metadata." :pathname
             pathname :identifier (and (stringp identifier) identifier)))
    (handler-case
     (papercut--validate-text note "assessment note"
      *papercut-assessment-note-limit*)
     (papercut-error (condition)
      (error 'papercut-error :message (autolith-error-message condition)
             :pathname pathname :identifier identifier)))
    (list identifier verdict note assessed-at)))

(serapeum/types:-> papercut--validate-closed-record (pathname list) string)
(defun papercut--validate-closed-record (pathname record)
  "Validate closure RECORD from PATHNAME and return its identifier."
  (let ((version    (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (closed-at  (getf (rest record) :closed-at))
        (resolution (getf (rest record) :resolution)))
    (unless
        (and (eql version *papercut-format-version*)
             (papercut--string-p identifier) (typep closed-at 'papercut-time))
      (error 'papercut-error :message
             "A papercut closure record has invalid metadata." :pathname
             pathname :identifier (and (stringp identifier) identifier)))
    (handler-case
     (papercut--validate-text resolution "closure resolution"
      *papercut-resolution-limit*)
     (papercut-error (condition)
      (error 'papercut-error :message (autolith-error-message condition)
             :pathname pathname :identifier identifier)))
    identifier))

(serapeum/types:-> papercut--record->papercut (pathname list) papercut)
(defun papercut--record->papercut (pathname record)
  "Validate and convert one portable papercut RECORD from PATHNAME."
  (let ((version             (getf (rest record) :version))
        (identifier          (getf (rest record) :id))
        (reported-at         (getf (rest record) :reported-at))
        (workspace           (getf (rest record) :workspace))
        (title               (getf (rest record) :title))
        (content             (getf (rest record) :content))
        (source-conversation (getf (rest record) :source-conversation)))
    (unless
        (and (eql version *papercut-format-version*)
             (papercut--string-p identifier) (typep reported-at 'papercut-time)
             (papercut--string-p workspace)
             (or (null (getf (rest record) :telemetry-id))
                 (papercut--export-id-p (getf (rest record) :telemetry-id)))
             (or (null source-conversation)
                 (papercut--string-p source-conversation)))
      (error 'papercut-error :message
             "A persistent papercut record has invalid metadata." :pathname
             pathname :identifier (and (stringp identifier) identifier)))
    (handler-case
     (make-instance 'papercut :identifier identifier :reported-at reported-at
                    :workspace workspace :title
                    (papercut--validate-text title "title"
                     *papercut-title-limit*)
                    :content
                    (papercut--validate-text content "content"
                     *papercut-content-limit*)
                    :diagnostic
                    (papercut--diagnostic (getf (rest record) :diagnostic))
                    :issue-kind
                    (papercut--issue-kind
                     (getf (rest record) :issue-kind "other"))
                    :tool                (papercut--tool (getf (rest record) :tool))
                    :telemetry-id        (getf (rest record) :telemetry-id)
                    :source-conversation source-conversation)
     (papercut-error (condition)
      (error 'papercut-error :message (autolith-error-message condition)
             :pathname pathname :identifier identifier)))))

(serapeum/types:-> papercut--reduce-record (pathname cons list) cons)
(defun papercut--reduce-record (pathname state record)
  "Apply one report, assessment, or closure to private lifecycle STATE."
  (let ((seen (first state)) (active (rest state)))
    (case (first record)
      (:papercut-telemetry
       (let* ((properties (rest record))
              (papercut   (gethash (getf properties :id) active))
              (identifier (getf properties :telemetry-id)))
         (unless
             (and papercut
                  (eql (getf properties :version) *papercut-format-version*)
                  (papercut--export-id-p identifier)
                  (null (papercut-telemetry-id papercut)))
           (error 'papercut-error :message
                  "Invalid papercut telemetry identity annotation." :pathname
                  pathname :identifier nil))
         (setf (papercut-telemetry-id papercut) identifier)))
      (:papercut
       (let* ((papercut   (papercut--record->papercut pathname record))
              (identifier (papercut-identifier papercut)))
         (when (gethash identifier seen)
           (error 'papercut-error :message
                  (format nil
                          "Persistent papercut identifier ~A occurs more than once."
                          identifier)
                  :pathname pathname :identifier identifier))
         (setf (gethash identifier seen) t
               (gethash identifier active) papercut)))
      (:papercut-assessed
       (destructuring-bind
           (identifier verdict note assessed-at)
           (papercut--validate-assessed-record pathname record)
         (let ((papercut (gethash identifier active)))
           (unless papercut
             (error 'papercut-error :message
                    (format nil
                            "Papercut assessment references inactive or unknown identifier ~A."
                            identifier)
                    :pathname pathname :identifier identifier))
           (setf (papercut-assessment-verdict papercut) verdict
                 (papercut-assessment-note papercut) note
                 (papercut-assessed-at papercut) assessed-at))))
      (:papercut-closed
       (let ((identifier (papercut--validate-closed-record pathname record)))
         (unless (gethash identifier seen)
           (error 'papercut-error :message
                  (format nil
                          "Papercut closure references unknown identifier ~A."
                          identifier)
                  :pathname pathname :identifier identifier))
         (unless (gethash identifier active)
           (error 'papercut-error :message
                  (format nil
                          "Papercut identifier ~A is closed more than once."
                          identifier)
                  :pathname pathname :identifier identifier))
         (when
             (member (papercut-assessment-verdict (gethash identifier active))
                     '(:worse :unchanged :too-early))
           (error 'papercut-error :message
                  (format nil
                          "Papercut ~A closure conflicts with its latest ~A assessment."
                          identifier
                          (papercut-assessment-verdict
                           (gethash identifier active)))
                  :pathname pathname :identifier identifier))
         (remhash identifier active)))
      (otherwise
       (error 'papercut-error :message
              (format nil "Unsupported persistent papercut record ~S."
                      (first record))
              :pathname pathname :identifier nil)))
    state))

(serapeum/types:-> papercut--sort-active (cons) list)
(defun papercut--sort-active (state)
  "Return active reports in deterministic newest-first presentation order."
  (sort
   (loop for papercut being the hash-values of (rest state)
         collect papercut)
   (lambda (left right)
     (or (> (papercut-reported-at left) (papercut-reported-at right))
         (and (= (papercut-reported-at left) (papercut-reported-at right))
              (string< (papercut-identifier left)
                       (papercut-identifier right)))))))

(serapeum/types:-> papercut--workspace-reports (papercut-context list) list)
(defun papercut--workspace-reports (papercut-context active)
  "Select current-workspace reports from ACTIVE transaction state."
  (let ((workspace (papercut--workspace papercut-context)))
    (remove-if-not
     (lambda (papercut) (string= workspace (papercut-workspace papercut)))
     active)))

(serapeum/types:-> papercut-list (papercut-context) list)
(defun papercut-list (papercut-context)
  "Return papercuts reported in CONFIGURATION's current workspace, newest first."
  (bordeaux-threads:with-lock-held (*papercut-lock*)
    (papercut--transact papercut-context
     (lambda (active)
       (values nil (papercut--workspace-reports papercut-context active)
               nil)))))

(serapeum/types:-> papercut-find (papercut-context string) (or null papercut))
(defun papercut-find (papercut-context identifier)
  "Return the exact active papercut IDENTIFIER in CONFIGURATION's workspace."
  (find identifier (papercut-list papercut-context) :test #'string= :key
        #'papercut-identifier))

(serapeum/types:-> papercut-resolve (papercut-context string)
                   (values (or null papercut)
                           (member :missing :ambiguous :found) list))
(defun papercut-resolve (papercut-context identifier)
  "Resolve an exact or unique identifier prefix in the current workspace.

The second value is :FOUND, :MISSING, or :AMBIGUOUS. The third value contains
matching reports for :AMBIGUOUS."
  (if (papercut--string-p identifier)
      (let* ((papercuts (papercut-list papercut-context))
             (matches
              (remove-if-not
               (lambda (papercut)
                 (uiop/utility:string-prefix-p identifier
                                               (papercut-identifier papercut)))
               papercuts)))
        (cond ((null matches)
               (values nil ':missing nil))
              ((null (rest matches))
               (values (first matches) ':found nil))
              (t
               (values nil ':ambiguous matches))))
      (values nil ':missing nil)))

(serapeum/types:-> papercut--tokens (string) list)
(defun papercut--tokens (text)
  "Return TEXT's distinct lowercase words of three or more characters.

Words keep inner dots, slashes, colons, hyphens, and underscores, so tool and
symbol names such as lisp.source survive, while surrounding punctuation drops."
  (let ((tokens nil) (start nil))
    (flet ((flush (end)
             (when start
               (let ((word
                      (string-trim "-_./:"
                                   (string-downcase (subseq text start end)))))
                 (when (>= (length word) 3)
                   (pushnew word tokens :test #'string=)))
               (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (or (alphanumericp character) (find character "-_./:"))
                   (unless start (setf start index))
                   (flush index))
            finally (flush (length text))))
    tokens))

(serapeum/types:-> papercut--similarity (list list) rational)
(defun papercut--similarity (left right)
  "Return the Jaccard similarity of word lists LEFT and RIGHT, from 0 to 1."
  (let ((union (union left right :test #'string=)))
    (if (null union)
        0
        (/ (length (intersection left right :test #'string=)) (length union)))))

(serapeum/types:-> papercut--same-words-p (list list) boolean)
(defun papercut--same-words-p (left right)
  "Return true when word lists LEFT and RIGHT hold the same words."
  (and (subsetp left right :test #'string=)
       (subsetp right left :test #'string=)))

(serapeum/types:-> papercut--duplicate
                   (papercut-context list &key (:title papercut-string)
                    (:content papercut-string)
                    (:source-conversation (or null string)) (:now integer))
                   (or null papercut))
(defun papercut--duplicate
       (papercut-context active &key title content source-conversation now)
  "Return the recent active report of this workspace that TITLE and CONTENT repeat.

A report repeats another filed within *PAPERCUT-DUPLICATE-WINDOW-SECONDS* when
their titles hold the same words, or when the words of title and body together
overlap at *PAPERCUT-DUPLICATE-SIMILARITY* or more. A report from
SOURCE-CONVERSATION filed within *PAPERCUT-DUPLICATE-SESSION-WINDOW-SECONDS*
only needs *PAPERCUT-DUPLICATE-SESSION-SIMILARITY*. The closest match wins."
  (let ((title-words (papercut--tokens title))
        (words
         (union (papercut--tokens title) (papercut--tokens content) :test
                #'string=))
        (best        nil)
        (best-score  0))
    (dolist
        (papercut (papercut--workspace-reports papercut-context active) best)
      (let ((age (abs (- now (papercut-reported-at papercut)))))
        (when (<= age *papercut-duplicate-window-seconds*)
          (let* ((other-title-words
                  (papercut--tokens (papercut-title papercut)))
                 (other-words
                  (union other-title-words
                         (papercut--tokens (papercut-content papercut)) :test
                         #'string=))
                 (score
                  (if (papercut--same-words-p title-words other-title-words)
                      1
                      (papercut--similarity words other-words)))
                 (threshold
                  (if (and source-conversation
                           (equal source-conversation
                                  (papercut-source-conversation papercut))
                           (<= age
                               *papercut-duplicate-session-window-seconds*))
                      *papercut-duplicate-session-similarity*
                      *papercut-duplicate-similarity*)))
            (when (and (>= score threshold) (> score best-score))
              (setf best papercut
                    best-score score))))))))

(serapeum/types:-> papercut--report-unlocked
                   (papercut-context &key (:title papercut-string)
                    (:content papercut-string) (:diagnostic (or null string))
                    (:source-conversation (or null string))
                    (:issue-kind string) (:tool (or null string))
                    (:active list))
                   (values list list boolean))
(defun papercut--report-unlocked
       (papercut-context
        &key title content diagnostic source-conversation (issue-kind "other")
        tool active)
  "Return records and (PAPERCUT DUPLICATE-P), reusing a recent active duplicate."
  (let* ((now (papercut-context-now papercut-context))
         (duplicate
          (papercut--duplicate papercut-context
           (remove-if-not
            (lambda (item)
              (and (equal (papercut-issue-kind item) issue-kind)
                   (equal (papercut-tool item) (papercut--tool tool))))
            active)
           :title title :content content :source-conversation
           source-conversation :now now)))
    (if duplicate
        (let ((records nil))
          (values records (list duplicate t) (not (null records))))
        (let ((papercut
               (make-instance 'papercut :identifier
                              (papercut-context-identifier papercut-context)
                              :reported-at now :workspace
                              (papercut--workspace papercut-context) :title
                              title :content content :diagnostic diagnostic
                              :issue-kind          (papercut--issue-kind issue-kind)
                              :tool (papercut--tool tool) :telemetry-id nil
                              :source-conversation source-conversation)))
          (values (list (papercut--record papercut)) (list papercut nil) t)))))

(serapeum/types:-> papercut-report
                   (papercut-context &key (:title string) (:content string)
                    (:diagnostic (or null string)) (:issue-kind string)
                    (:tool (or null string))
                    (:source-conversation (or null string)))
                   (values papercut boolean))
(defun papercut-report
       (papercut-context
        &key title content diagnostic source-conversation (issue-kind "other")
        tool)
  "Record a report in the trusted workspace. Return the report and whether it was a duplicate."
  (let ((validated-title
         (papercut--validate-text title "title" *papercut-title-limit*))
        (validated-content
         (papercut--validate-text content "content" *papercut-content-limit*)))
    (setf diagnostic (papercut--diagnostic diagnostic))
    (unless
        (or (null source-conversation)
            (papercut--string-p source-conversation))
      (error 'papercut-error :message
             "Papercut source conversation must be a non-empty string."
             :pathname (papercut-context-path papercut-context) :identifier
             nil))
    (destructuring-bind
        (papercut duplicate-p)
        (bordeaux-threads:with-lock-held (*papercut-lock*)
          (papercut--transact papercut-context
           (lambda (active)
             (papercut--report-unlocked papercut-context :title validated-title
              :content validated-content :diagnostic diagnostic :issue-kind
              issue-kind :tool tool :source-conversation source-conversation
              :active                                           active))))
      nil
      (values papercut duplicate-p))))

(serapeum/types:-> papercut--assess-unlocked
                   (papercut-context papercut-string &key (:active list)
                    (:verdict (member :improved :worse :unchanged :too-early))
                    (:note papercut-string) (:diagnostic (or null string)))
                   (values list papercut boolean))
(defun papercut--assess-unlocked
       (papercut-context identifier &key active verdict note diagnostic)
  "Return the assessment event and its updated report from private state."
  (let ((papercut
         (find identifier (papercut--workspace-reports papercut-context active)
               :test #'string= :key #'papercut-identifier))
        (assessed-at (papercut-context-now papercut-context)))
    (unless papercut
      (error 'papercut-error :message
             (format nil "No active papercut ~A exists in this workspace."
                     identifier)
             :pathname (papercut-context-path papercut-context) :identifier
             identifier))
    (setf (papercut-assessment-verdict papercut) verdict
          (papercut-assessment-note papercut) note
          (papercut-assessed-at papercut) assessed-at)
    (values
     (list
      (papercut--assessed-record identifier :verdict verdict :note note
       :assessed-at assessed-at :diagnostic diagnostic))
     papercut t)))

(serapeum/types:-> papercut-assess
                   (papercut-context string &key (:verdict keyword)
                    (:note string) (:diagnostic (or null string)))
                   papercut)
(defun papercut-assess
       (papercut-context identifier &key verdict note diagnostic)
  "Append an effectiveness assessment for active papercut IDENTIFIER."
  (setf diagnostic (papercut--diagnostic diagnostic))
  (unless (papercut--string-p identifier)
    (error 'papercut-error :message
           "Papercut identifier must be a non-empty string." :pathname
           (papercut-context-path papercut-context) :identifier nil))
  (unless (member verdict '(:improved :worse :unchanged :too-early))
    (error 'papercut-error :message
           "Papercut assessment verdict must be improved, worse, unchanged, or too-early."
           :pathname (papercut-context-path papercut-context) :identifier
           identifier))
  (let ((validated-note
         (papercut--validate-text note "assessment note"
          *papercut-assessment-note-limit*)))
    (let ((papercut
           (bordeaux-threads:with-lock-held (*papercut-lock*)
             (papercut--transact papercut-context
              (lambda (active)
                (papercut--assess-unlocked papercut-context identifier :active
                 active :verdict verdict :note validated-note :diagnostic
                 diagnostic))))))
      nil
      papercut)))

(serapeum/types:-> papercut--mark-closed-unlocked
                   (papercut-context papercut-string papercut-string &key
                    (:active list) (:diagnostic (or null string)))
                   (values list papercut boolean))
(defun papercut--mark-closed-unlocked
       (papercut-context identifier resolution &key active diagnostic)
  "Return a closure event after checking the workspace and assessment policy."
  (let* ((workspace (papercut--workspace papercut-context))
         (papercut
          (find-if
           (lambda (candidate)
             (and (string= identifier (papercut-identifier candidate))
                  (string= workspace (papercut-workspace candidate))))
           active)))
    (unless papercut
      (error 'papercut-error :message
             (format nil "No active papercut ~A exists in this workspace."
                     identifier)
             :pathname (papercut-context-path papercut-context) :identifier
             identifier))
    (when
        (member (papercut-assessment-verdict papercut)
                '(:worse :unchanged :too-early))
      (error 'papercut-error :message
             (format nil
                     "Papercut ~A cannot be closed while its latest assessment is ~A."
                     identifier (papercut-assessment-verdict papercut))
             :pathname (papercut-context-path papercut-context) :identifier
             identifier))
    (values
     (list
      (papercut--closed-record identifier resolution
       (papercut-context-now papercut-context) :diagnostic diagnostic))
     papercut t)))

(serapeum/types:-> papercut-mark-closed
                   (papercut-context string &key (:resolution string)
                    (:diagnostic (or null string)))
                   papercut)
(defun papercut-mark-closed
       (papercut-context identifier &key resolution diagnostic)
  "Close active papercut IDENTIFIER with a durable RESOLUTION and return it."
  (setf diagnostic (papercut--diagnostic diagnostic))
  (unless (papercut--string-p identifier)
    (error 'papercut-error :message
           "Papercut identifier must be a non-empty string." :pathname
           (papercut-context-path papercut-context) :identifier nil))
  (let ((validated-resolution
         (papercut--validate-text resolution "closure resolution"
          *papercut-resolution-limit*)))
    (let ((papercut
           (bordeaux-threads:with-lock-held (*papercut-lock*)
             (papercut--transact papercut-context
              (lambda (active)
                (papercut--mark-closed-unlocked papercut-context identifier
                 validated-resolution :active active :diagnostic
                 diagnostic))))))
      nil
      papercut)))

(serapeum/types:-> papercut-timestamp-string (papercut-time) string)
(defun papercut-timestamp-string (papercut-time)
  "Return TIMESTAMP as an ISO-8601 UTC string."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time papercut-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month date
            hour minute second)))

(serapeum/types:-> papercut-short-identifier (papercut) string)
(defun papercut-short-identifier (papercut)
  "Return the stable abbreviated identifier shown in the terminal."
  (subseq (papercut-identifier papercut) 0
          (min *papercut-short-identifier-length*
               (length (papercut-identifier papercut)))))

(serapeum/types:-> papercut-call-source (papercut) string)
(defun papercut-call-source (papercut)
  "Return the canonical Lisp call that opens PAPERCUT."
  (format nil "(papercut ~S)" (papercut-short-identifier papercut)))

(serapeum/types:-> papercut-excerpt (string integer) string)
(defun papercut-excerpt (content limit)
  "Return a single-line prefix of CONTENT no longer than LIMIT characters."
  (let* ((single-line
          (with-output-to-string (stream)
            (loop with spacing-p = nil
                  for character across content
                  if (find character '(#\  #\Tab #\Newline #\Return))
                  do (unless spacing-p
                       (write-char #\  stream)
                       (setf spacing-p t)) else
                  do (write-char character stream) (setf spacing-p nil))))
         (trimmed (string-trim '(#\ ) single-line)))
    (if (<= (length trimmed) limit)
        trimmed
        (format nil "~A..." (subseq trimmed 0 (max 0 (- limit 3)))))))

;;;; -- Guarded Lifecycle and Receipt Log --

(serapeum/types:-> papercut--workspace (papercut-context) string)
(defun papercut--workspace (context)
  "Return the trusted workspace identity used in durable records."
  (papercut--validate-text (papercut-context-workspace context) "workspace" 4096))

(serapeum/types:-> papercut--receipt-p (t) boolean)
(defun papercut--receipt-p (record)
  "Validate complete receipt metadata, canonical payload, and public result snapshot."
  (handler-case
      (let* ((properties (rest record))
             (payload    (getf properties :payload))
             (snapshot   (getf properties :result))
             (report     (papercut--record->papercut #P"papercuts.sexp" (getf snapshot :report)))
             (verdict    (getf snapshot :verdict))
             (revision   (getf properties :revision)))
        (and (eq (first record) ':papercut-operation)
             (= (length properties) 14)
             (eql (getf properties :version) 1)
             (papercut--string-p (getf properties :session))
             (<= (length (getf properties :session)) 1024)
             (papercut--string-p (getf properties :id))
             (<= (length (getf properties :id)) 1024)
             (equal (getf properties :workspace) (papercut-workspace report))
             (stringp revision) (= (length revision) 64)
             (every (lambda (character) (digit-char-p character 16)) revision)
             (= (length snapshot) 10)
             (member (getf snapshot :status) '("active" "closed") :test #'equal)
             (if verdict
                 (and (member verdict '(:improved :worse :unchanged :too-early))
                      (papercut--validate-text (getf snapshot :note) "note" 1000)
                      (typep (getf snapshot :assessed-at) 'papercut-time))
                 (and (null (getf snapshot :note)) (null (getf snapshot :assessed-at))))
             (member (length payload) '(4 5))
             (destructuring-bind (operation id expected fields &optional collection-revision) payload
               (and (stringp id) (stringp expected)
                     (or (= (length payload) 4)
                         (and (eq operation ':create) (papercut--string-p collection-revision)
                              (<= (length collection-revision) 1024)))
                    (if (eq operation ':create)
                        (and (zerop (length id)) (zerop (length expected)))
                        (and (member operation '(:assess :close))
                             (papercut--string-p id) (papercut--string-p expected)))
                    (equal (loop for tail on fields by (lambda (tail) (rest (rest tail))) collect (first tail))
                           (case operation
                             (:create
                              '(:title :content :diagnostic :issue-kind :tool))
                             (:assess
                              '(:verdict :note :diagnostic))
                             (:close
                              '(:resolution :diagnostic))))
                    (progn (papercut--diagnostic (getf fields :diagnostic)) t)))
             t))
    (error () nil)))

(serapeum/types:-> papercut--store (papercut-context) sexp-store:log-store)
(defun papercut--store (context)
  "Describe the single lifecycle log, including atomic operation receipts."
  (let ((path (papercut-context-path context)))
    (make-instance
     'sexp-store:log-store
     :pathname         path
     :lock-pathname    (merge-pathnames "papercuts.lock"
                                        (uiop:pathname-directory-pathname path))
     :header           '(:papercuts :version 1)
     :header-validator (lambda (form)
                         (sexp-store:record-check form :tag ':papercuts :versions '(1)))
     :validator        (lambda (form) (and (consp form) (keywordp (first form))))
     :initial-state    (lambda ()
                         (list (cons (make-hash-table :test #'equal)
                                     (make-hash-table :test #'equal)) nil))
     :reducer
     (lambda (state record)
       (if (eq (first record) ':papercut-operation)
           (progn
             (unless (papercut--receipt-p record)
               (error 'papercut-error :message "Invalid papercut operation receipt."
                      :pathname path :identifier nil))
             (when (or (>= (length (second state)) 16384)
                       (find-if
                        (lambda (old)
                          (and (equal (getf (rest old) :session) (getf (rest record) :session))
                               (equal (getf (rest old) :id) (getf (rest record) :id))))
                        (second state)))
               (error 'papercut-error :message "Invalid or repeated papercut operation receipt."
                      :pathname path :identifier nil))
             (push record (second state)))
           (papercut--reduce-record path (first state) record))
       state)
     :finalizer        (lambda (state)
                         (list (papercut--sort-active (first state)) (second state))))))

(serapeum/types:-> papercut-call-with-transaction (papercut-context function) (values t list))
(defun papercut-call-with-transaction (context update)
  "Call UPDATE with (active-reports receipts) under the private store lock.

UPDATE returns lifecycle and receipt records, its result, and a changed flag.
The store commits all returned records atomically. Return UPDATE's result and
then the committed store state."
  (handler-case
      (multiple-value-bind (result state)
          (sexp-store:store-transact (papercut--store context) update)
        (values result state))
    (sexp-store:store-error (cause)
      (error 'papercut-error :message (sexp-store:store-error-message cause)
             :pathname (sexp-store:store-error-pathname cause) :identifier nil))))

(serapeum/types:-> papercut--transact (papercut-context function) (values t list))
(defun papercut--transact (context update)
  "Run UPDATE on active reports. Return its result and committed store state."
  (papercut-call-with-transaction context
    (lambda (state) (funcall update (first state)))))
