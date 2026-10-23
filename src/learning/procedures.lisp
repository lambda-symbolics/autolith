(in-package #:autolith)

;;;; -- Learned Procedures --

;;; A procedure is a tool-call sequence learned from turns that did the same
;;; work, kept as auditable data:
;;;
;;;   (:procedure :id "make-sales-report"
;;;    :parameters ((:name "file" :type :string))
;;;    :input ("Make" "the" "sales" "report" "for" (:param "file" "" "."))
;;;    :steps ((:tool "sales.load"
;;;             :arguments (:object ("file" (:text "data/" "file" ".csv"))))
;;;            (:tool "sales.report"
;;;             :arguments (:object ("table" (:from 0 ("table")))))))
;;;
;;; Argument templates are constants (:JSON TEXT), parameters (:PARAM NAME),
;;; text around a parameter (:TEXT PREFIX NAME SUFFIX), values taken from an
;;; earlier step's JSON result (:FROM STEP PATH), and objects and arrays of
;;; templates. INPUT, when present, matches requests the procedure fits: its
;;; words are literal or bind a parameter, or any word, between a fixed
;;; prefix and suffix. Running a procedure calls its tools through the
;;; ordinary tool path, so each call keeps its own authorization and
;;; contracts.
;;;
;;; Procedures live in the knowledge log. A learned procedure is a candidate
;;; until it is admitted; consecutive failed runs suspend it until it is
;;; admitted again, and a retired procedure is never learned again.

(defparameter *procedure-step-limit* 32
  "The most tool calls one procedure makes.")

(defparameter *procedure-parameter-limit* 8
  "The most parameters one procedure takes.")

(defparameter *procedure-suspension-limit* 2
  "How many consecutive failed runs suspend an admitted procedure.")

(defparameter *procedure-failure-limit* 400
  "The most characters of a failed run's reason kept in the log.")

(deftype procedure-parameter-type ()
  "The kinds of value a procedure parameter accepts."
  '(member :string :integer :number :boolean))

(deftype procedure-status ()
  "Where a procedure stands: learned, approved to run, stopped by failures, or rejected."
  '(member :candidate :admitted :suspended :retired))


;;;; -- Validation --

(-> procedure-template-p (t) boolean)
(defun procedure-template-p (template)
  "Return true when TEMPLATE has the structure of an argument template."
  (and (consp template)
       (proper-list-p template)
       (case (first template)
         (:json
          (and (= (length template) 2) (stringp (second template))))
         (:param
          (and (= (length template) 2) (non-empty-string-p (second template))))
         (:text
          (and (= (length template) 4)
               (stringp (second template))
               (non-empty-string-p (third template))
               (stringp (fourth template))
               (plusp (+ (length (second template)) (length (fourth template))))))
         (:from
          (and (= (length template) 3)
               (typep (second template) '(integer 0))
               (proper-list-p (third template))
               (every (lambda (key) (or (stringp key) (typep key '(integer 0))))
                      (third template))))
         (:object
          (every (lambda (entry)
                   (and (proper-list-p entry)
                        (= (length entry) 2)
                        (stringp (first entry))
                        (procedure-template-p (second entry))))
                 (rest template)))
         (:array
          (every #'procedure-template-p (rest template)))
         (otherwise
          nil))
       t))

(-> procedure-parameter-p (t) boolean)
(defun procedure-parameter-p (parameter)
  "Return true when PARAMETER is a name, a value type, and an optional required suffix."
  (and (proper-list-p parameter)
       (evenp (length parameter))
       (knowledge-rule-name-p (getf parameter :name))
       (typep (getf parameter :type) 'procedure-parameter-type)
       (or (null (getf parameter :suffix))
           (and (eq (getf parameter :type) ':string)
                (non-empty-string-p (getf parameter :suffix))))
       t))

(-> procedure-p (t) boolean)
(defun procedure-p (procedure)
  "Return true when PROCEDURE is well formed and refers only to its own parameters and earlier steps."
  (and (proper-list-p procedure)
       (eq (first procedure) :procedure)
       (evenp (length (rest procedure)))
       (let* ((properties (rest procedure))
              (parameters (getf properties :parameters))
              (steps (getf properties :steps)))
         (and (knowledge-rule-name-p (getf properties :id))
              (proper-list-p parameters)
              (<= (length parameters) *procedure-parameter-limit*)
              (every #'procedure-parameter-p parameters)
              (let ((names (mapcar (lambda (parameter) (getf parameter :name)) parameters)))
                (and (= (length names) (length (remove-duplicates names :test #'string=)))
                     (proper-list-p steps)
                     (<= 1 (length steps) *procedure-step-limit*)
                     (loop for step in steps
                           for index from 0
                           always (and (proper-list-p step)
                                       (evenp (length step))
                                       (non-empty-string-p (getf step :tool))
                                       (procedure-template-p (getf step :arguments))
                                       (eq (first (getf step :arguments)) :object)
                                       (procedure--references-valid-p (getf step :arguments)
                                                                      names index)))
                     (procedure--input-valid-p (getf properties :input) names)))
              (or (null (getf properties :description))
                  (stringp (getf properties :description)))))
       t))

(-> procedure-record-valid-p (t) boolean)
(defun procedure-record-valid-p (record)
  "Return true when RECORD is a well-formed procedure definition, admission, retirement, or run."
  (let ((properties (rest record)))
    (and (eq (getf properties :scope) ':workspace)
         (non-empty-string-p (getf properties :workspace))
         (typep (getf properties :time) 'timestamp)
         (ecase (first record)
           (:procedure-defined
            (and (procedure-p (getf properties :procedure))
                 (proper-list-p (getf properties :examples))
                 (proper-list-p (getf properties :baseline))
                 (proper-list-p (getf properties :evaluation))))
           (:procedure-admitted
            (and (knowledge-rule-name-p (getf properties :id))
                 (typep (getf properties :procedure-version) '(integer 1))))
           (:procedure-retired
            (knowledge-rule-name-p (getf properties :id)))
           (:procedure-run
            (and (knowledge-rule-name-p (getf properties :id))
                 (typep (getf properties :procedure-version) '(integer 1))
                 (member (getf properties :status) '(:ok :failed))
                 (member (getf properties :caller) '(:user :model))
                 (typep (getf properties :steps) '(integer 0))
                 (or (null (getf properties :failure))
                     (and (stringp (getf properties :failure))
                          (<= (length (getf properties :failure))
                              *procedure-failure-limit*))))))
         t)))


;;;; -- Templates --

(-> procedure-json-equal-p (t t) boolean)
(defun procedure-json-equal-p (left right)
  "Return true when decoded JSON values LEFT and RIGHT are equal, ignoring object key order."
  (cond
    ((and (json-object-p left) (json-object-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (loop for key being the hash-keys of left using (hash-value value)
                always (multiple-value-bind (other present-p) (gethash key right)
                         (and present-p (procedure-json-equal-p value other))))))
    ((and (vectorp left) (not (stringp left)) (vectorp right) (not (stringp right)))
     (and (= (length left) (length right))
          (every #'procedure-json-equal-p left right)))
    ((and (numberp left) (numberp right))
     (= left right))
    (t
     (equal left right))))

(-> procedure-json-path-value (t list) (values t boolean))
(defun procedure-json-path-value (value path)
  "Return the part of decoded JSON VALUE at PATH and whether it exists.

PATH lists object keys and array indexes from the outside in."
  (block walk
    (dolist (key path)
      (cond
        ((and (stringp key) (json-object-p value))
         (multiple-value-bind (next present-p) (gethash key value)
           (unless present-p
             (return-from walk (values nil nil)))
           (setf value next)))
        ((and (integerp key) (vectorp value) (not (stringp value)) (< key (length value)))
         (setf value (aref value key)))
        (t
         (return-from walk (values nil nil)))))
    (values value t)))

(-> procedure-template-instantiate (list list list) t)
(defun procedure-template-instantiate (template bindings outputs)
  "Return the JSON value TEMPLATE describes, given parameter BINDINGS and earlier OUTPUTS.

BINDINGS maps parameter names to values. OUTPUTS lists each earlier step's
decoded JSON result, first step first. A :FROM path the output lacks signals
KNOWLEDGE-ERROR with code :MISSING-OUTPUT."
  (ecase (first template)
    (:json
     (json-decode (second template)))
    (:param
     (procedure--binding bindings (second template)))
    (:text
     (format nil "~A~A~A" (second template)
             (procedure--binding bindings (third template))
             (fourth template)))
    (:from
     (multiple-value-bind (value present-p)
         (if (< (second template) (length outputs))
             (procedure-json-path-value (nth (second template) outputs) (third template))
             (values nil nil))
       (unless present-p
         (knowledge--fail ':missing-output (third template)
                          "Step ~D's result has no ~{~A~^.~}."
                          (1+ (second template)) (third template)))
       value))
    (:object
     (let ((object (json-object)))
       (loop for (key value) in (rest template)
             do (setf (gethash key object)
                      (procedure-template-instantiate value bindings outputs)))
       object))
    (:array
     (coerce (mapcar (lambda (item) (procedure-template-instantiate item bindings outputs))
                     (rest template))
             'vector))))

(-> procedure-template-match (list t list list) (values list boolean))
(defun procedure-template-match (template value bindings outputs)
  "Match decoded JSON VALUE against TEMPLATE, extending parameter BINDINGS.

Return the extended bindings and whether VALUE fits. OUTPUTS are the earlier
steps' decoded results that :FROM templates must agree with."
  (block match
    (ecase (first template)
      (:json
       (values bindings (procedure-json-equal-p value (json-decode (second template)))))
      (:param
       (procedure--bind bindings (second template) value))
      (:text
       (let ((middle (procedure--text-middle value (second template) (fourth template))))
         (if middle
             (procedure--bind bindings (third template) middle)
             (values bindings nil))))
      (:from
       (multiple-value-bind (expected present-p)
           (if (< (second template) (length outputs))
               (procedure-json-path-value (nth (second template) outputs) (third template))
               (values nil nil))
         (values bindings (and present-p (procedure-json-equal-p expected value)))))
      (:object
       (unless (and (json-object-p value)
                    (= (hash-table-count value) (length (rest template))))
         (return-from match (values bindings nil)))
       (loop for (key item) in (rest template)
             do (multiple-value-bind (found present-p) (gethash key value)
                  (unless present-p
                    (return-from match (values bindings nil)))
                  (multiple-value-bind (next matched-p)
                      (procedure-template-match item found bindings outputs)
                    (unless matched-p
                      (return-from match (values bindings nil)))
                    (setf bindings next))))
       (values bindings t))
      (:array
       (unless (and (vectorp value) (not (stringp value))
                    (= (length value) (length (rest template))))
         (return-from match (values bindings nil)))
       (loop for item in (rest template)
             for found across value
             do (multiple-value-bind (next matched-p)
                    (procedure-template-match item found bindings outputs)
                  (unless matched-p
                    (return-from match (values bindings nil)))
                  (setf bindings next)))
       (values bindings t)))))

(-> procedure-template-text (list) string)
(defun procedure-template-text (template)
  "Return TEMPLATE as JSON-like text: parameters as {NAME} and earlier results as <call N PATH>."
  (ecase (first template)
    (:json
     (second template))
    (:param
     (format nil "{~A}" (second template)))
    (:text
     (let ((prefix (json-encode (second template)))
           (suffix (json-encode (fourth template))))
       (format nil "~A{~A}~A" (subseq prefix 0 (1- (length prefix))) (third template)
               (subseq suffix 1))))
    (:from
     (format nil "<call ~D ~{~A~^.~}>" (1+ (second template)) (third template)))
    (:object
     (format nil "{~{~A~^,~}}"
             (mapcar (lambda (entry)
                       (format nil "~A:~A" (json-encode (first entry))
                               (procedure-template-text (second entry))))
                     (rest template))))
    (:array
     (format nil "[~{~A~^,~}]" (mapcar #'procedure-template-text (rest template))))))

(-> procedure-input-text (list) (option string))
(defun procedure-input-text (procedure)
  "Return PROCEDURE's request pattern with {NAME} for parameters and * for any word, or NIL."
  (let ((input (getf (rest procedure) :input)))
    (and input
         (format nil "~{~A~^ ~}"
                 (mapcar (lambda (element)
                           (cond
                             ((stringp element) element)
                             ((eq (first element) :param)
                              (format nil "~A{~A}~A" (third element) (second element)
                                      (fourth element)))
                             (t
                              (format nil "~A*~A" (second element) (third element)))))
                         input)))))

(-> procedure-parameter-accepts-p (list t) boolean)
(defun procedure-parameter-accepts-p (parameter value)
  "Return true when VALUE has PARAMETER's type and ends with its required suffix."
  (and (ecase (getf parameter :type)
         (:string
          (and (non-empty-string-p value)
               (let ((suffix (getf parameter :suffix)))
                 (or (null suffix)
                     (and (> (length value) (length suffix))
                          (string= suffix value :start2 (- (length value) (length suffix))))))))
         (:integer (integerp value))
         (:number (realp value))
         (:boolean (or (eq value t) (eq value :json-false))))
       t))

(-> procedure-match-input (list string) (values list boolean))
(defun procedure-match-input (procedure text)
  "Match request TEXT against PROCEDURE's input pattern.

Return the parameter bindings it supplies and whether it matched. Words are
compared without regard to case; bound values must suit their parameters."
  (let ((pattern (getf (rest procedure) :input))
        (words (procedure-words text))
        (bindings nil))
    (block match
      (unless (and pattern (= (length pattern) (length words)))
        (return-from match (values nil nil)))
      (loop for element in pattern
            for word in words
            do (cond
                 ((stringp element)
                  (unless (string-equal element word)
                    (return-from match (values nil nil))))
                 (t
                  (destructuring-bind (kind &rest details) element
                    (multiple-value-bind (prefix suffix)
                        (if (eq kind :param)
                            (values (second details) (third details))
                            (values (first details) (second details)))
                      (let ((middle (procedure--text-middle word prefix suffix :empty-ends-p t)))
                        (unless middle
                          (return-from match (values nil nil)))
                        (when (eq kind :param)
                          (let ((parameter (procedure-parameter procedure (first details))))
                            (unless (procedure-parameter-accepts-p
                                     parameter
                                     (procedure--parse-value middle (getf parameter :type)))
                              (return-from match (values nil nil)))
                            (multiple-value-bind (next matched-p)
                                (procedure--bind bindings (first details)
                                                 (procedure--parse-value
                                                  middle (getf parameter :type)))
                              (unless matched-p
                                (return-from match (values nil nil)))
                              (setf bindings next))))))))))
      (values bindings t))))

(-> procedure-words (string) list)
(defun procedure-words (text)
  "Return TEXT's whitespace-separated words."
  (let ((words nil)
        (start nil))
    (loop for index from 0 to (length text)
          for character = (and (< index (length text)) (char text index))
          do (if (and character
                   (not (find character '(#\Space #\Tab #\Newline #\Return #\Page))))
                 (unless start
                   (setf start index))
                 (when start
                   (push (subseq text start index) words)
                   (setf start nil))))
    (nreverse words)))

(-> procedure-parameter (list string) (option list))
(defun procedure-parameter (procedure name)
  "Return PROCEDURE's parameter named NAME, or NIL."
  (find name (getf (rest procedure) :parameters)
        :key (lambda (parameter) (getf parameter :name))
        :test #'string=))

(-> procedure-describe-call (list list) string)
(defun procedure-describe-call (procedure bindings)
  "Return the procedure.run arguments for PROCEDURE with parameter BINDINGS as JSON text."
  (let ((parameters (json-object)))
    (dolist (binding bindings)
      (setf (gethash (first binding) parameters) (rest binding)))
    (json-encode (json-object "id" (getf (rest procedure) :id) "parameters" parameters))))


;;;; -- Stored Procedures --

(-> procedure-entry-reduce ((option list) list) list)
(defun procedure-entry-reduce (entry record)
  "Return procedure ENTRY after the validated procedure RECORD, without modifying ENTRY.

An entry holds :ID, :VERSIONS, the :ADMITTED version, :RETIRED-P, and run
counts. A record naming an unknown procedure or version signals a corrupt
KNOWLEDGE-ERROR."
  (let ((properties (rest record))
        (entry (copy-list entry)))
    (flet ((require-version (version)
             (unless (and entry (<= 1 version (length (getf entry :versions))))
               (knowledge--fail ':corrupt (getf properties :id)
                                "~(~A~) names unknown procedure ~A version ~A."
                                (first record) (getf properties :id) version))))
      (ecase (first record)
        (:procedure-defined
         (let ((procedure (getf properties :procedure)))
           (unless entry
             (setf entry (list :id (getf (rest procedure) :id)
                               :workspace (getf properties :workspace)
                               :versions nil
                               :admitted nil
                               :retired-p nil
                               :runs 0
                               :failures 0
                               :consecutive-failures 0
                               :last-failure nil
                               :user-runs 0)))
           (setf (getf entry :versions)
                 (append (getf entry :versions)
                         (list (list :version (1+ (length (getf entry :versions)))
                                     :procedure procedure
                                     :examples (getf properties :examples)
                                     :baseline (getf properties :baseline)
                                     :evaluation (getf properties :evaluation)
                                     :time (getf properties :time)))))))
        (:procedure-admitted
         (require-version (getf properties :procedure-version))
         (setf (getf entry :admitted) (getf properties :procedure-version)
               (getf entry :retired-p) nil
               (getf entry :consecutive-failures) 0))
        (:procedure-retired
         (require-version 1)
         (setf (getf entry :admitted) nil
               (getf entry :retired-p) t))
        (:procedure-run
         (require-version (getf properties :procedure-version))
         (incf (getf entry :runs))
         (when (eq (getf properties :caller) ':user)
           (incf (getf entry :user-runs)))
         (if (eq (getf properties :status) ':ok)
             (setf (getf entry :consecutive-failures) 0)
             (progn
               (incf (getf entry :failures))
               (incf (getf entry :consecutive-failures))
               (setf (getf entry :last-failure) (getf properties :failure))))))
      entry)))

(-> procedure-entry-status (list) procedure-status)
(defun procedure-entry-status (entry)
  "Return where the procedure ENTRY describes stands."
  (cond
    ((getf entry :retired-p) ':retired)
    ((null (getf entry :admitted)) ':candidate)
    ((>= (getf entry :consecutive-failures) *procedure-suspension-limit*) ':suspended)
    (t ':admitted)))

(-> procedure-entry-version (list &optional (option integer)) list)
(defun procedure-entry-version (entry &optional version)
  "Return ENTRY's VERSION record, by default the newest one."
  (if version
      (nth (1- version) (getf entry :versions))
      (first (last (getf entry :versions)))))

(-> procedure-entry-runnable-version (list) (option list))
(defun procedure-entry-runnable-version (entry)
  "Return the version record ENTRY runs: the admitted one, or NIL when none may run."
  (and (eq (procedure-entry-status entry) ':admitted)
       (procedure-entry-version entry (getf entry :admitted))))

(-> knowledge-base-procedure (knowledge-base string string) (option list))
(defun knowledge-base-procedure (base workspace identifier)
  "Return WORKSPACE's procedure entry named IDENTIFIER in BASE, or NIL."
  (gethash (list workspace identifier) (knowledge-base-procedures-table base)))

(-> knowledge-base-procedures (knowledge-base string) list)
(defun knowledge-base-procedures (base workspace)
  "Return WORKSPACE's procedure entries in BASE, ordered by name."
  (let ((entries nil))
    (maphash (lambda (key entry)
               (when (equal (first key) workspace)
                 (push entry entries)))
             (knowledge-base-procedures-table base))
    (sort entries #'string< :key (lambda (entry) (getf entry :id)))))

(-> procedure-define
    (configuration list &key (:examples list) (:baseline list) (:evaluation list)
                             (:source-conversation (option string)))
    (values string integer boolean))
(defun procedure-define
    (configuration procedure &key examples baseline evaluation source-conversation)
  "Record PROCEDURE for CONFIGURATION's workspace unless its newest version is already equal.

EXAMPLES lists the (:CONVERSATION :SEQUENCE) of the turns it was learned from,
BASELINE their average (:REQUESTS :TOKENS), and EVALUATION how it was checked.
Return its name, version, and whether a version was added."
  (unless (procedure-p procedure)
    (knowledge--fail ':invalid-procedure procedure "The procedure is malformed."))
  (let ((workspace (knowledge--workspace configuration))
        (identifier (getf (rest procedure) :id)))
    (values-list
     (knowledge-transact
      configuration
      (lambda (base)
        (let* ((entry (knowledge-base-procedure base workspace identifier))
               (latest (and entry (procedure-entry-version entry))))
          (if (and latest (equal (getf latest :procedure) procedure))
              (values nil (list identifier (getf latest :version) nil) nil)
              (values (list (list :procedure-defined
                                  :version *knowledge-format-version*
                                  :scope ':workspace
                                  :workspace workspace
                                  :procedure procedure
                                  :examples examples
                                  :baseline baseline
                                  :evaluation evaluation
                                  :time (get-universal-time)
                                  :source-conversation source-conversation))
                      (list identifier (1+ (length (getf entry :versions))) t)
                      t))))))))

(-> procedure-admit (configuration string &key (:version (option integer))) integer)
(defun procedure-admit (configuration identifier &key version)
  "Admit VERSION of procedure IDENTIFIER, by default its newest, so it may run.

Admitting clears a suspension and a retirement. Return the admitted version."
  (let ((workspace (knowledge--workspace configuration)))
    (knowledge-transact
     configuration
     (lambda (base)
       (let ((entry (knowledge-base-procedure base workspace identifier)))
         (unless entry
           (knowledge--fail ':unknown-procedure identifier "No procedure is named ~A." identifier))
         (let ((version (or version (getf (procedure-entry-version entry) :version))))
           (unless (<= 1 version (length (getf entry :versions)))
             (knowledge--fail ':unknown-procedure identifier
                              "Procedure ~A has no version ~D." identifier version))
           (if (and (eq (procedure-entry-status entry) ':admitted)
                    (eql (getf entry :admitted) version))
               (values nil version nil)
               (values (list (list :procedure-admitted
                                   :scope ':workspace
                                   :workspace workspace
                                   :id identifier
                                   :procedure-version version
                                   :time (get-universal-time)))
                       version
                       t))))))))

(-> procedure-retire (configuration string) boolean)
(defun procedure-retire (configuration identifier)
  "Retire procedure IDENTIFIER so it neither runs nor is learned again.

Return true when it was not already retired."
  (let ((workspace (knowledge--workspace configuration)))
    (knowledge-transact
     configuration
     (lambda (base)
       (let ((entry (knowledge-base-procedure base workspace identifier)))
         (unless entry
           (knowledge--fail ':unknown-procedure identifier "No procedure is named ~A." identifier))
         (if (getf entry :retired-p)
             (values nil nil nil)
             (values (list (list :procedure-retired
                                 :scope ':workspace
                                 :workspace workspace
                                 :id identifier
                                 :time (get-universal-time)))
                     t
                     t)))))))

(-> procedure-record-run
    (configuration string &key (:version integer) (:status keyword) (:steps integer)
                               (:caller keyword) (:failure (option string))
                               (:conversation (option string)))
    null)
(defun procedure-record-run
    (configuration identifier &key version status steps caller failure conversation)
  "Append one run of procedure IDENTIFIER VERSION to the ledger.

STATUS is :OK or :FAILED, STEPS how many calls succeeded, and CALLER :USER or :MODEL."
  (let ((workspace (knowledge--workspace configuration)))
    (knowledge-transact
     configuration
     (lambda (base)
       (declare (ignore base))
       (values (list (list :procedure-run
                           :scope ':workspace
                           :workspace workspace
                           :id identifier
                           :procedure-version version
                           :status status
                           :steps steps
                           :caller caller
                           :failure (and failure
                                         (subseq failure 0 (min (length failure)
                                                                *procedure-failure-limit*)))
                           :conversation conversation
                           :time (get-universal-time)))
               nil
               t))))
  nil)


;;;; -- Private Functions --

(-> procedure--references-valid-p (list list integer) boolean)
(defun procedure--references-valid-p (template names step)
  "Return true when TEMPLATE names only parameters in NAMES and steps before STEP."
  (and (ecase (first template)
         (:json t)
         (:param (member (second template) names :test #'string=))
         (:text (member (third template) names :test #'string=))
         (:from (< (second template) step))
         (:object (every (lambda (entry) (procedure--references-valid-p (second entry) names step))
                         (rest template)))
         (:array (every (lambda (item) (procedure--references-valid-p item names step))
                        (rest template))))
       t))

(-> procedure--input-valid-p (t list) boolean)
(defun procedure--input-valid-p (input names)
  "Return true when INPUT is absent or a list of words and holes binding parameters in NAMES."
  (and (proper-list-p input)
       (every (lambda (element)
                (or (non-empty-string-p element)
                    (and (proper-list-p element)
                         (case (first element)
                           (:param (and (= (length element) 4)
                                        (member (second element) names :test #'string=)
                                        (stringp (third element))
                                        (stringp (fourth element))))
                           (:any (and (= (length element) 3)
                                      (stringp (second element))
                                      (stringp (third element))))
                           (otherwise nil)))))
              input)
       t))

(-> procedure--binding (list string) t)
(defun procedure--binding (bindings name)
  "Return parameter NAME's value in BINDINGS, signaling KNOWLEDGE-ERROR when it has none."
  (let ((binding (assoc name bindings :test #'string=)))
    (unless binding
      (knowledge--fail ':missing-parameter name "Parameter ~A has no value." name))
    (rest binding)))

(-> procedure--bind (list string t) (values list boolean))
(defun procedure--bind (bindings name value)
  "Bind NAME to VALUE in BINDINGS unless it is bound to a different value.

Return the bindings and whether VALUE agreed."
  (let ((binding (assoc name bindings :test #'string=)))
    (cond
      ((null binding) (values (acons name value bindings) t))
      ((procedure-json-equal-p (rest binding) value) (values bindings t))
      (t (values bindings nil)))))

(-> procedure--text-middle (t string string &key (:empty-ends-p boolean)) (option string))
(defun procedure--text-middle (value prefix suffix &key empty-ends-p)
  "Return the non-empty part of string VALUE between PREFIX and SUFFIX, or NIL.

Unless EMPTY-ENDS-P, PREFIX or SUFFIX must be non-empty."
  (and (stringp value)
       (or empty-ends-p (plusp (+ (length prefix) (length suffix))))
       (> (length value) (+ (length prefix) (length suffix)))
       (string= prefix value :end2 (length prefix))
       (string= suffix value :start2 (- (length value) (length suffix)))
       (subseq value (length prefix) (- (length value) (length suffix)))))

(-> procedure--parse-value (string procedure-parameter-type) t)
(defun procedure--parse-value (text type)
  "Return request word TEXT decoded as TYPE, or TEXT when it is not valid JSON."
  (case type
    ((:integer :number :boolean)
     (handler-case (json-decode text)
       (json-error () text)))
    (otherwise
     text)))
