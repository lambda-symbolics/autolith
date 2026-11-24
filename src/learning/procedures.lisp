(in-package #:autolith)

;;;; -- Learned Procedures --

(defparameter *procedure-suspension-limit* 2
  "How many consecutive failed runs suspend an active procedure.")

(defparameter *procedure-failure-limit* 400
  "The most characters of a failed run's reason kept in the log.")

(deftype procedure-parameter-type ()
  "The kinds of value a procedure parameter accepts."
  '(member :string :integer :number :boolean))

(deftype procedure-status ()
  "Whether a procedure is active, suspended, retired, or inactive legacy history."
  '(member :inactive :active :suspended :retired))


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
         (:concat
          (and (rest template)
               (every (lambda (part)
                        (and (procedure-template-p part)
                             (member (first part) '(:json :param :text :from))))
                      (rest template))))
         (otherwise
          nil))
       t))

(-> procedure-parameter-p (t) boolean)
(defun procedure-parameter-p (parameter)
  "Return true when PARAMETER is a name, a value type, and an optional required suffix."
  (and (plist-schema-p parameter :keyword-keys-p nil :allow-duplicate-keys t)
       (knowledge-rule-name-p (getf parameter :name))
       (typep (getf parameter :type) 'procedure-parameter-type)
       (or (null (getf parameter :suffix))
           (and (eq (getf parameter :type) ':string)
                (non-empty-string-p (getf parameter :suffix))))
       t))

(-> procedure-p (t) boolean)
(defun procedure-p (procedure)
  "Return true when PROCEDURE is well formed and refers only to its own parameters and earlier steps."
  (and (consp procedure)
       (eq (first procedure) :procedure)
       (plist-schema-p (rest procedure) :keyword-keys-p nil :allow-duplicate-keys t)
       (let* ((properties (rest procedure))
              (parameters (getf properties :parameters))
              (steps      (getf properties :steps)))
         (and (knowledge-rule-name-p (getf properties :id))
              (proper-list-p parameters)
              (every #'procedure-parameter-p parameters)
              (let ((names (mapcar (lambda (parameter) (getf parameter :name)) parameters)))
                (and (= (length names) (length (remove-duplicates names :test #'string=)))
                     (proper-list-p steps)
                     steps
                     (loop for step in steps
                           for index from 0
                           always (and (plist-schema-p step :keyword-keys-p nil :allow-duplicate-keys t)
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
                 ;; Untagged definitions and admission records replay until 0.69.0.
                 (member (getf properties :activation) '(nil :automatic))
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
             (json-path-get-present (nth (second template) outputs) (third template))
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
             'vector))
    (:concat
     (format nil "~{~A~}"
             (mapcar (lambda (part)
                       (procedure--text-piece
                        (procedure-template-instantiate part bindings outputs)))
                     (rest template))))))

(-> procedure-template-match (list t &key (:bindings list) (:outputs list)) (values list boolean))
(defun procedure-template-match (template value &key bindings outputs)
  "Match decoded JSON VALUE against TEMPLATE, extending parameter BINDINGS.

Return the extended bindings and whether VALUE fits. OUTPUTS are the earlier
steps' decoded results that :FROM templates must agree with."
  (block match
    (ecase (first template)
      (:json
       (values bindings (json-value-equal-p value (json-decode (second template)))))
      (:param
       (procedure--bind bindings (second template) value))
      (:text
       (if-let ((middle (procedure--text-middle value (second template) (fourth template))))
           (procedure--bind bindings (third template) middle)
           (values bindings nil)))
      (:from
       (multiple-value-bind (expected present-p)
           (if (< (second template) (length outputs))
               (json-path-get-present (nth (second template) outputs) (third template))
               (values nil nil))
         (values bindings (and present-p (json-value-equal-p expected value)))))
      (:object
       (unless (and (json-object-p value)
                    (= (hash-table-count value) (length (rest template))))
         (return-from match (values bindings nil)))
       (loop for (key item) in (rest template)
             do (multiple-value-bind (found present-p) (gethash key value)
                  (unless present-p
                    (return-from match (values bindings nil)))
                  (multiple-value-bind (next matched-p)
                      (procedure-template-match item found :bindings bindings :outputs outputs)
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
                    (procedure-template-match item found :bindings bindings :outputs outputs)
                  (unless matched-p
                    (return-from match (values bindings nil)))
                  (setf bindings next)))
       (values bindings t))
      (:concat
       (if (stringp value)
           (procedure--concat-match (rest template) value
                                    :start    0
                                    :bindings bindings
                                    :outputs  outputs)
           (values bindings nil))))))

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
     (format nil "[~{~A~^,~}]" (mapcar #'procedure-template-text (rest template))))
    (:concat
     (format nil "\"~{~A~}\""
             (mapcar (lambda (part)
                       (let ((text (procedure-template-text part)))
                         (if (eql (char text 0) #\")
                             (subseq text 1 (1- (length text)))
                             text)))
                     (rest template))))))

(-> procedure-input-text (list) (option string))
(defun procedure-input-text (procedure)
  "Return PROCEDURE's request pattern with {NAME} for parameters and * for any word, or NIL."
  (let ((input (getf (rest procedure) :input)))
    (and input
         (format nil "~{~A~^ ~}"
                 (mapcar (lambda (element)
                           (cond
                             ((stringp element)
                              element)
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
         (:integer
          (integerp value))
         (:number
          (realp value))
         (:boolean
          (or (eq value t) (eq value :json-false))))
       t))

(-> procedure-match-input (list string) (values list boolean))
(defun procedure-match-input (procedure text)
  "Match request TEXT against PROCEDURE's input pattern.

Return the parameter bindings it supplies and whether it matched. Words are
compared without regard to case; bound values must suit their parameters."
  (let ((pattern  (getf (rest procedure) :input))
        (words    (procedure-words text))
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
  (serapeum:tokens text))

(-> procedure-parameter (list string) (option list))
(defun procedure-parameter (procedure name)
  "Return PROCEDURE's parameter named NAME, or NIL."
  (find name (getf (rest procedure) :parameters)
        :key  (lambda (parameter) (getf parameter :name))
        :test #'string=))

(-> procedure-describe-call (list list) string)
(defun procedure-describe-call (procedure bindings)
  "Return the procedure.run arguments for PROCEDURE with parameter BINDINGS as JSON text."
  (let ((parameters (json-object)))
    (dolist (binding bindings)
      (setf (gethash (first binding) parameters) (rest binding)))
    (json-encode (json-object "id" (getf (rest procedure) :id) "parameters" parameters))))


;;;; -- Authored Procedures --

(-> procedure-from-json (t) list)
(defun procedure-from-json (object)
  "Return the procedure described by the decoded JSON OBJECT.

OBJECT contains an id, an optional description, optional parameters and the
steps. Each parameter contains a name, an optional type and an optional
description. Each step contains a tool and its arguments. A string in the
arguments refers to a parameter as {NAME} and to the result of an earlier step
N as <call N PATH>, where PATH is an optional dot-separated path into a JSON
result. Signal KNOWLEDGE-ERROR with code :INVALID-PROCEDURE if OBJECT does
not describe a valid procedure."
  (flet ((fail (control &rest arguments)
           (apply #'knowledge--fail ':invalid-procedure object control arguments)))
    (unless (json-object-p object)
      (fail "A procedure is a JSON object."))
    (let* ((identifier  (json-get object "id"))
           (description (json-get object "description"))
           (parameters  (procedure--json-parameters (json-get object "parameters")))
           (names       (mapcar (lambda (parameter) (getf parameter :name)) parameters))
           (steps       (json-get object "steps")))
      (unless (knowledge-rule-name-p identifier)
        (fail "The id ~S does not start with a lowercase letter, or it contains characters other than lowercase letters, digits, hyphens and dots."
              identifier))
      (unless (or (null description) (stringp description))
        (fail "The description is a string."))
      (unless (and (vectorp steps) (not (stringp steps)) (plusp (length steps)))
        (fail "The steps are a non-empty array."))
      (list :procedure
            :id          identifier
            :description description
            :parameters  parameters
            :steps       (loop for step across steps
                               for index from 0
                               collect (procedure--json-step step names index))))))

(-> procedure-definition-report
    (list &key (:version integer) (:added-p boolean) (:status procedure-status))
    string)
(defun procedure-definition-report (procedure &key version added-p status)
  "Return the text reporting PROCEDURE as VERSION with STATUS, and whether VERSION is new by ADDED-P.

The text contains every step and the parameters that no step uses."
  (let* ((properties (rest procedure))
         (used       (mapcan (lambda (step) (procedure--template-parameters (getf step :arguments)))
                             (getf properties :steps)))
         (unused     (remove-if (lambda (name) (member name used :test #'string=))
                                (mapcar (lambda (parameter) (getf parameter :name))
                                        (getf properties :parameters)))))
    (with-output-to-string (stream)
      (format stream "Procedure ~A version ~D ~:[already has this definition~;is defined~]. ~A"
              (getf properties :id) version added-p
              (ecase status
                (:active
                 "Run it with procedure.run.")
                (:suspended
                 "It is suspended after failed runs. It runs again only after a changed definition.")
                (:retired
                 "It is retired, so it does not run.")))
      (loop for step in (getf properties :steps)
            for index from 1
            do (format stream "~%~D. ~A ~A" index (getf step :tool)
                       (procedure-template-text (getf step :arguments))))
      (when unused
        (format stream "~%No step uses parameter~P ~{~A~^, ~}." (length unused) unused)))))


;;;; -- Stored Procedures --

(-> procedure-entry-reduce ((option list) list) list)
(defun procedure-entry-reduce (entry record)
  "Return procedure ENTRY after the validated procedure RECORD, without modifying ENTRY.

An entry holds :ID, :VERSIONS, the :ACTIVE version, :RETIRED-P, and run
counts. A record naming an unknown procedure or version signals a corrupt
KNOWLEDGE-ERROR."
  (let ((properties (rest record))
        (entry      (copy-list entry)))
    (flet ((require-version (version)
             (unless (and entry (<= 1 version (length (getf entry :versions))))
               (knowledge--fail ':corrupt (getf properties :id)
                                "~(~A~) names unknown procedure ~A version ~A."
                                (first record) (getf properties :id) version))))
      (ecase (first record)
        (:procedure-defined
         (let ((procedure (getf properties :procedure)))
           (unless entry
             (setf entry (list :id                   (getf (rest procedure) :id)
                               :workspace            (getf properties :workspace)
                               :versions             nil
                               :active               nil
                               :retired-p            nil
                               :runs                 0
                               :failures             0
                               :consecutive-failures 0
                               :last-failure         nil
                               :user-runs            0)))
           (setf (getf entry :versions)
                 (append (getf entry :versions)
                         (list (list :version    (1+ (length (getf entry :versions)))
                                     :procedure  procedure
                                     :examples   (getf properties :examples)
                                     :baseline   (getf properties :baseline)
                                     :evaluation (getf properties :evaluation)
                                     :time       (getf properties :time)))))
           (when (and (eq (getf properties :activation) ':automatic)
                      (not (getf entry :retired-p)))
             (setf (getf entry :active) (length (getf entry :versions))
                   (getf entry :consecutive-failures) 0))))
        ;; Replay-only admission history, removable with the 0.69.0 log migration.
        (:procedure-admitted
         (require-version (getf properties :procedure-version))
         (setf (getf entry :active) (getf properties :procedure-version)
               (getf entry :retired-p) nil
               (getf entry :consecutive-failures) 0))
        (:procedure-retired
         (require-version 1)
         (setf (getf entry :active) nil
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
    ((getf entry :retired-p)
     ':retired)
    ((null (getf entry :active))
     ':inactive)
    ((>= (getf entry :consecutive-failures) *procedure-suspension-limit*)
     ':suspended)
    (t
     ':active)))

(-> procedure-entry-version (list &optional (option integer)) list)
(defun procedure-entry-version (entry &optional version)
  "Return ENTRY's VERSION record, by default the newest one."
  (if version
      (nth (1- version) (getf entry :versions))
      (first (last (getf entry :versions)))))

(-> procedure-entry-runnable-version (list) (option list))
(defun procedure-entry-runnable-version (entry)
  "Return the active version record ENTRY runs, or NIL when none may run."
  (and (eq (procedure-entry-status entry) ':active)
       (procedure-entry-version entry (getf entry :active))))

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
  "Record and activate PROCEDURE unless its newest version is already equal.

A corrected version resets the failure streak. An unchanged definition leaves
suspension intact, and a retired procedure cannot reactivate this way.

EXAMPLES lists the (:CONVERSATION :SEQUENCE) of the turns it was learned from,
BASELINE their average (:REQUESTS :TOKENS), and EVALUATION how it was checked.
Return its name, version, and whether a version was added."
  (unless (procedure-p procedure)
    (knowledge--fail ':invalid-procedure procedure "The procedure is malformed."))
  (let ((workspace  (knowledge--workspace configuration))
        (identifier (getf (rest procedure) :id)))
    (values-list
     (knowledge-transact
      configuration
      (lambda (base)
        (let* ((entry  (knowledge-base-procedure base workspace identifier))
               (latest (and entry (procedure-entry-version entry))))
          (if (and latest (equal (getf latest :procedure) procedure)
                   (or (eql (getf entry :active) (getf latest :version))
                       (getf entry :retired-p)))
              (values nil (list identifier (getf latest :version) nil) nil)
              (values (list (list :procedure-defined
                                  :version *knowledge-format-version*
                                  :scope               ':workspace
                                  :workspace           workspace
                                  :procedure           procedure
                                  :examples            examples
                                  :baseline            baseline
                                  :evaluation          evaluation
                                  :activation          ':automatic
                                  :time                (get-universal-time)
                                  :source-conversation source-conversation))
                      (list identifier (1+ (length (getf entry :versions))) t)
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
                                 :id        identifier
                                 :time      (get-universal-time)))
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
                           :workspace         workspace
                           :id                identifier
                           :procedure-version version
                           :status            status
                           :steps             steps
                           :caller            caller
                           :failure           (and failure
                                                   (subseq failure 0 (min (length failure)
                                                                          *procedure-failure-limit*)))
                           :conversation      conversation
                           :time              (get-universal-time)))
               nil
               t))))
  nil)


;;;; -- Private Functions --

(-> procedure--references-valid-p (list list integer) boolean)
(defun procedure--references-valid-p (template names step)
  "Return true when TEMPLATE names only parameters in NAMES and steps before STEP."
  (and (ecase (first template)
         (:json
          t)
         (:param
          (member (second template) names :test #'string=))
         (:text
          (member (third template) names :test #'string=))
         (:from
          (< (second template) step))
         (:object
          (every (lambda (entry) (procedure--references-valid-p (second entry) names step))
                 (rest template)))
         ((:array :concat)
          (every (lambda (item) (procedure--references-valid-p item names step))
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
                           (:param
                            (and (= (length element) 4)
                                 (member (second element) names :test #'string=)
                                 (stringp (third element))
                                 (stringp (fourth element))))
                           (:any
                            (and (= (length element) 3)
                                 (stringp (second element))
                                 (stringp (third element))))
                           (otherwise
                            nil)))))
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
      ((null binding)
       (values (acons name value bindings) t))
      ((json-value-equal-p (rest binding) value)
       (values bindings t))
      (t
       (values bindings nil)))))

(-> procedure--json-parameters (t) list)
(defun procedure--json-parameters (parameters)
  "Return the procedure parameters described by the decoded JSON array PARAMETERS."
  (flet ((fail (control &rest arguments)
           (apply #'knowledge--fail ':invalid-procedure parameters control arguments)))
    (unless (or (null parameters) (and (vectorp parameters) (not (stringp parameters))))
      (fail "The parameters are an array."))
    (let ((result
            (loop for parameter across (or parameters #())
                  for name = (and (json-object-p parameter) (json-get parameter "name"))
                  for type = (if (json-object-p parameter)
                                 (json-get parameter "type" "string")
                                 "string")
                  for description = (and (json-object-p parameter)
                                         (json-get parameter "description"))
                  do (unless (knowledge-rule-name-p name)
                       (fail "The parameter name ~S does not start with a lowercase letter, or it contains characters other than lowercase letters, digits, hyphens and dots."
                             name))
                     (unless (find type '(:string :integer :number :boolean) :test #'string-equal)
                       (fail "The type of parameter ~A is string, integer, number or boolean."
                             name))
                     (unless (or (null description) (stringp description))
                       (fail "The description of parameter ~A is a string." name))
                  collect (list* :name name
                                 :type (find type '(:string :integer :number :boolean)
                                             :test #'string-equal)
                                 (and description (list :description description))))))
      (when-let ((duplicate (find-if (lambda (parameter)
                                       (< 1 (count (getf parameter :name) result
                                                   :key  (lambda (other) (getf other :name))
                                                   :test #'string=)))
                                     result)))
        (fail "Two parameters are named ~A." (getf duplicate :name)))
      result)))

(-> procedure--json-step (t list integer) list)
(defun procedure--json-step (step names index)
  "Return procedure step INDEX described by the decoded JSON STEP, given the parameter NAMES."
  (unless (and (json-object-p step) (non-empty-string-p (json-get step "tool")))
    (knowledge--fail ':invalid-procedure step "Step ~D is an object with a tool." (1+ index)))
  (let ((arguments (json-get step "arguments" (json-object))))
    (unless (json-object-p arguments)
      (knowledge--fail ':invalid-procedure step "The arguments of step ~D are an object."
                       (1+ index)))
    (list :tool      (json-get step "tool")
          :arguments (procedure--json-template arguments names index))))

(-> procedure--json-template (t list integer) list)
(defun procedure--json-template (value names step)
  "Return the template of the JSON VALUE in the arguments of STEP, given the parameter NAMES."
  (cond
    ((stringp value)
     (procedure--string-template value names step))
    ((json-object-p value)
     (cons :object
           (loop for key being the hash-keys of value using (hash-value item)
                 collect (list key (procedure--json-template item names step)))))
    ((vectorp value)
     (cons :array
           (map 'list (lambda (item) (procedure--json-template item names step)) value)))
    (t
     (list :json (json-encode value)))))

(-> procedure--string-template (string list integer) list)
(defun procedure--string-template (text names step)
  "Return the template of TEXT in the arguments of STEP.

TEXT refers to the parameters in NAMES as {NAME} and to the results of the
steps before STEP as <call N PATH>. Other text is literal."
  (let ((parts   nil)
        (literal ""))
    (loop for piece in (cl-ppcre:split "(\\{[a-z][a-z0-9.-]*\\}|<call [0-9]+(?: [^<>]*)?>)" text
                                       :with-registers-p t)
          for reference-p = nil then (not reference-p)
          for reference = (and reference-p (procedure--string-reference piece names step))
          do (if reference
                 (progn
                   (when (plusp (length literal))
                     (push (list :json (json-encode literal)) parts))
                   (push reference parts)
                   (setf literal ""))
                 (setf literal (concatenate 'string literal piece))))
    (when (or (plusp (length literal)) (null parts))
      (push (list :json (json-encode literal)) parts))
    (if (rest parts)
        (cons :concat (nreverse parts))
        (first parts))))

(-> procedure--string-reference (string list integer) (option list))
(defun procedure--string-reference (text names step)
  "Return the template TEXT refers to: a parameter in NAMES or the result of a step before STEP.

Return NIL if TEXT refers to neither."
  (if (eql (char text 0) #\{)
      (let ((name (subseq text 1 (1- (length text)))))
        (and (member name names :test #'string=)
             (list :param name)))
      (cl-ppcre:register-groups-bind ((#'parse-integer number) path)
          ("^<call ([0-9]+)(?: (.*))?>$" text)
        (and (<= 1 number step)
             (list :from (1- number)
                   (loop for key in (cl-ppcre:split "\\." (or path ""))
                         unless (zerop (length key))
                           collect (if (every #'digit-char-p key)
                                       (parse-integer key)
                                       key)))))))

(-> procedure--template-parameters (list) list)
(defun procedure--template-parameters (template)
  "Return the names of the parameters TEMPLATE refers to."
  (case (first template)
    (:param
     (list (second template)))
    (:text
     (list (third template)))
    (:object
     (mapcan (lambda (entry) (procedure--template-parameters (second entry))) (rest template)))
    ((:array :concat)
     (mapcan #'procedure--template-parameters (rest template)))))

(-> procedure--text-piece (t) string)
(defun procedure--text-piece (value)
  "Return JSON VALUE as text: a string as itself and anything else as JSON."
  (if (stringp value)
      value
      (json-encode value)))

(-> procedure--concat-match
    (list string &key (:start (integer 0)) (:bindings list) (:outputs list))
    (values list boolean))
(defun procedure--concat-match (parts text &key start bindings outputs)
  "Match TEXT from START against the concatenation of the templates PARTS, extending BINDINGS.

Return the extended bindings and whether TEXT fits. A parameter matches any
non-empty text, and the other parts must match their own text exactly."
  (block match
    (when (null parts)
      (return-from match (values bindings (= start (length text)))))
    (let ((part (first parts)))
      (if (eq (first part) :param)
          (loop for end from (1+ start) to (length text)
                do (multiple-value-bind (next bound-p)
                       (procedure--bind bindings (second part) (subseq text start end))
                     (when bound-p
                       (multiple-value-bind (found matched-p)
                           (procedure--concat-match (rest parts) text
                                                    :start    end
                                                    :bindings next
                                                    :outputs  outputs)
                         (when matched-p
                           (return-from match (values found t)))))))
          (let* ((piece (handler-case
                            (procedure--text-piece
                             (procedure-template-instantiate part bindings outputs))
                          (knowledge-error ()
                            (return-from match (values bindings nil)))))
                 (end   (+ start (length piece))))
            (when (and (<= end (length text))
                       (string= piece text :start2 start :end2 end))
              (return-from match
                (procedure--concat-match (rest parts) text
                                         :start    end
                                         :bindings bindings
                                         :outputs  outputs)))))
      (values bindings nil))))

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
