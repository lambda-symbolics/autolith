(in-package #:autolith-supervisor)

(defparameter *knowledge-contract-pattern-limit*
  8
  "The most requirements, effects, or observations one contract lists.")

(serapeum/types:-> knowledge--reference-p (t) boolean)
(defun knowledge--reference-p (value)
  "Return true when VALUE refers to a call argument or a result field."
  (and (stringp value)
       (or (uiop/utility:string-prefix-p "$arg." value)
           (uiop/utility:string-prefix-p "$result." value) (string= value "$result"))
       t))

(serapeum/types:-> knowledge-contract-p (t) boolean)
(defun knowledge-contract-p (contract)
  "Return true when CONTRACT has the structure of a validated contract."
  (and (alexandria:proper-list-p contract) (eq (first contract) :contract)
       (knowledge--properties-p (rest contract))
       (let ((properties (rest contract)))
         (and (knowledge-rule-name-p (getf properties :id))
              (non-empty-string-p (getf properties :tool))
              (every (lambda (entry) (and (consp entry) (non-empty-string-p (first entry))))
                     (getf properties :when))
              (every #'consp (getf properties :requires))
              (every #'consp (getf properties :effects))
              (every (lambda (basis) (typep basis 'knowledge-basis)) (getf properties :accepts))
              (every
               (lambda (observation)
                 (and (knowledge--properties-p observation)
                      (knowledge-predicate-name-p (getf observation :predicate))
                      (alexandria:proper-list-p (getf observation :arguments))))
               (getf properties :observes))))
       t))

(serapeum/types:-> knowledge--accepted-bases (t string) list)
(defun knowledge--accepted-bases (accepts identifier)
  "Return the bases ACCEPTS names, input text or keywords, or NIL for the default."
  (let ((items (coerce (or accepts #()) 'list)))
    (mapcar
     (lambda (item)
       (or
        (find item '(:observation :user-assertion :assumption :model-proposal) :test
              (lambda (value basis)
                (or (eq value basis)
                    (and (stringp value) (string-equal value (symbol-name basis))))))
        (knowledge--fail ':invalid-contract identifier
                         "accepts lists observation, user-assertion, assumption, or model-proposal.")))
     items)))

(serapeum/types:-> knowledge-contract-admitted-p (list) boolean)
(defun knowledge-contract-admitted-p (entry)
  "Return true when ENTRY's exact current contract has an admission receipt."
  (let ((admission (getf entry :admission)))
    (and admission (equal (getf admission :contract) (getf entry :contract))
         (eql (getf admission :contract-version) (getf entry :contract-version)) t)))

(serapeum/types:-> knowledge-base-contracts
                   (knowledge-base string &key (:workspace (option string))) list)
(defun knowledge-base-contracts (base tool &key workspace)
  "Return admitted contracts describing TOOL visible from WORKSPACE."
  (loop for entry being the hash-values of (knowledge-base-contracts-table base)
        for contract = (getf (getf entry :admission) :contract)
        when (and contract
                  (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
                  (string= (getf (rest contract) :tool) tool))
        collect contract))

(serapeum/types:-> knowledge--task-call-p (t) boolean)
(defun knowledge--task-call-p (value)
  "Return true when VALUE is a task name followed by variables or values."
  (and (alexandria:proper-list-p value) (knowledge-rule-name-p (first value))
       (every (lambda (item) (or (knowledge-variable-p item) (knowledge-term-p item)))
              (rest value))
       t))

(serapeum/types:-> knowledge-method-p (t) boolean)
(defun knowledge-method-p (method)
  "Return true when METHOD has the structure of a validated method."
  (and (alexandria:proper-list-p method) (eq (first method) :method) (evenp (length (rest method)))
       (let ((properties (rest method)))
         (and (knowledge-rule-name-p (getf properties :id))
              (knowledge--task-call-p (getf properties :task))
              (alexandria:proper-list-p (getf properties :when))
              (alexandria:proper-list-p (getf properties :steps)) (getf properties :steps)
              (every
               (lambda (step)
                 (and (alexandria:proper-list-p step)
                      (case (first step)
                        (:task
                         (knowledge--task-call-p (second step)))
                        (:step
                         (non-empty-string-p (second step)))
                        (otherwise
                         nil))))
               (getf properties :steps))))
       t))

(defparameter *knowledge-diagnosis-hypothesis-limit* 4 "The most hypotheses one diagnosis weighs.")

(defparameter *knowledge-diagnosis-probe-limit* 8 "The most probes one diagnosis lists.")

(serapeum/types:-> knowledge-diagnosis-p (t) boolean)
(defun knowledge-diagnosis-p (diagnosis)
  "Return true when DIAGNOSIS has the structure of a validated diagnosis."
  (and (alexandria:proper-list-p diagnosis) (eq (first diagnosis) :diagnosis)
       (evenp (length (rest diagnosis)))
       (let ((properties (rest diagnosis)))
         (and (knowledge-rule-name-p (getf properties :id))
              (every #'knowledge-variable-p (getf properties :parameters))
              (<= 1 (length (getf properties :hypotheses)) *knowledge-diagnosis-hypothesis-limit*)
              (every
               (lambda (hypothesis)
                 (and (alexandria:proper-list-p hypothesis)
                      (knowledge-rule-name-p (getf hypothesis :id)) (getf hypothesis :predicts)
                      (every #'consp (getf hypothesis :predicts))
                      (or (null (getf hypothesis :repair)) (stringp (getf hypothesis :repair)))))
               (getf properties :hypotheses))
              (<= (length (getf properties :probes)) *knowledge-diagnosis-probe-limit*)
              (every
               (lambda (probe)
                 (and (alexandria:proper-list-p probe) (non-empty-string-p (getf probe :tool))
                      (every
                       (lambda (argument)
                         (and (alexandria:proper-list-p argument) (= (length argument) 2)
                              (stringp (first argument))))
                       (getf probe :arguments))
                      (realp (getf probe :cost)) (plusp (getf probe :cost))))
               (getf properties :probes))))
       t))

(defparameter *procedure-step-limit* 32 "The most tool calls one procedure makes.")

(defparameter *procedure-parameter-limit* 8 "The most parameters one procedure takes.")

(defparameter *procedure-suspension-limit*
  2
  "How many consecutive failed runs suspend an admitted procedure.")

(defparameter *procedure-failure-limit*
  400
  "The most characters of a failed run's reason kept in the log.")

(deftype procedure-parameter-type ()
  "The kinds of value a procedure parameter accepts."
  '(member :string :integer :number :boolean))

(deftype procedure-status ()
  "Where a procedure stands: learned, approved to run, stopped by failures, or rejected."
  '(member :candidate :admitted :suspended :retired))

(serapeum/types:-> procedure-template-p (t) boolean)
(defun procedure-template-p (template)
  "Return true when TEMPLATE has the structure of an argument template."
  (and (consp template) (alexandria:proper-list-p template)
       (case (first template)
         (:json
          (and (= (length template) 2) (stringp (second template))))
         (:param
          (and (= (length template) 2) (non-empty-string-p (second template))))
         (:text
          (and (= (length template) 4) (stringp (second template))
               (non-empty-string-p (third template)) (stringp (fourth template))
               (plusp (+ (length (second template)) (length (fourth template))))))
         (:from
          (and (= (length template) 3) (typep (second template) '(integer 0))
               (alexandria:proper-list-p (third template))
               (every (lambda (key) (or (stringp key) (typep key '(integer 0))))
                      (third template))))
         (:object
          (every
           (lambda (entry)
             (and (alexandria:proper-list-p entry) (= (length entry) 2) (stringp (first entry))
                  (procedure-template-p (second entry))))
           (rest template)))
         (:array
          (every #'procedure-template-p (rest template)))
         (otherwise
          nil))
       t))

(serapeum/types:-> procedure-parameter-p (t) boolean)
(defun procedure-parameter-p (parameter)
  "Return true when PARAMETER is a name, a value type, and an optional required suffix."
  (and (alexandria:proper-list-p parameter) (evenp (length parameter))
       (knowledge-rule-name-p (getf parameter :name))
       (typep (getf parameter :type) 'procedure-parameter-type)
       (or (null (getf parameter :suffix))
           (and (eq (getf parameter :type) ':string)
                (non-empty-string-p (getf parameter :suffix))))
       t))

(serapeum/types:-> procedure-p (t) boolean)
(defun procedure-p (procedure)
  "Return true when PROCEDURE is well formed and refers only to its own parameters and earlier steps."
  (and (alexandria:proper-list-p procedure) (eq (first procedure) :procedure)
       (evenp (length (rest procedure)))
       (let* ((properties (rest procedure))
              (parameters (getf properties :parameters))
              (steps      (getf properties :steps)))
         (and (knowledge-rule-name-p (getf properties :id)) (alexandria:proper-list-p parameters)
              (<= (length parameters) *procedure-parameter-limit*)
              (every #'procedure-parameter-p parameters)
              (let ((names (mapcar (lambda (parameter) (getf parameter :name)) parameters)))
                (and (= (length names) (length (remove-duplicates names :test #'string=)))
                     (alexandria:proper-list-p steps) (<= 1 (length steps) *procedure-step-limit*)
                     (loop for step in steps
                           for index from 0
                           always (and (alexandria:proper-list-p step) (evenp (length step))
                                       (non-empty-string-p (getf step :tool))
                                       (procedure-template-p (getf step :arguments))
                                       (eq (first (getf step :arguments)) :object)
                                       (procedure--references-valid-p (getf step :arguments) names
                                                                      index)))
                     (procedure--input-valid-p (getf properties :input) names)))
              (or (null (getf properties :description)) (stringp (getf properties :description)))))
       t))

(serapeum/types:-> procedure-record-valid-p (t) boolean)
(defun procedure-record-valid-p (record)
  "Return true when RECORD is a well-formed procedure definition, admission, retirement, or run."
  (let ((properties (rest record)))
    (and (eq (getf properties :scope) ':workspace)
         (non-empty-string-p (getf properties :workspace))
         (typep (getf properties :time) 'timestamp)
         (ecase (first record)
           (:procedure-defined
            (and (procedure-p (getf properties :procedure))
                 (alexandria:proper-list-p (getf properties :examples))
                 (alexandria:proper-list-p (getf properties :baseline))
                 (alexandria:proper-list-p (getf properties :evaluation))))
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
                          (<= (length (getf properties :failure)) *procedure-failure-limit*))))))
         t)))

(serapeum/types:-> procedure-entry-reduce ((option list) list) list)
(defun procedure-entry-reduce (entry record)
  "Return procedure ENTRY after the validated procedure RECORD, without modifying ENTRY.

An entry holds :ID, :VERSIONS, the :ADMITTED version, :RETIRED-P, and run
counts. A record naming an unknown procedure or version signals a corrupt
KNOWLEDGE-ERROR."
  (let ((properties (rest record)) (entry (copy-list entry)))
    (flet ((require-version (version)
             (unless (and entry (<= 1 version (length (getf entry :versions))))
               (knowledge--fail ':corrupt (getf properties :id)
                                "~(~A~) names unknown procedure ~A version ~A." (first record)
                                (getf properties :id) version))))
      (ecase (first record)
        (:procedure-defined
         (let ((procedure (getf properties :procedure)))
           (unless entry
             (setf entry
                     (list :id (getf (rest procedure) :id) :workspace (getf properties :workspace)
                           :versions nil :admitted nil :retired-p nil :runs 0 :failures 0
                           :consecutive-failures 0 :last-failure nil :user-runs 0)))
           (setf (getf entry :versions)
                 (append (getf entry :versions)
                         (list
                          (list :version (1+ (length (getf entry :versions))) :procedure
                                procedure :examples (getf properties :examples) :baseline
                                (getf properties :baseline) :evaluation
                                (getf properties :evaluation) :time (getf properties :time)))))))
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
         (when (eq (getf properties :caller) ':user) (incf (getf entry :user-runs)))
         (if (eq (getf properties :status) ':ok)
             (setf (getf entry :consecutive-failures) 0)
             (progn
              (incf (getf entry :failures))
              (incf (getf entry :consecutive-failures))
              (setf (getf entry :last-failure) (getf properties :failure))))))
      entry)))

(serapeum/types:-> procedure--references-valid-p (list list integer) boolean)
(defun procedure--references-valid-p (template names step)
  "Return true when TEMPLATE names only parameters in NAMES and steps before STEP."
  (and
   (ecase (first template)
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
     (:array
      (every (lambda (item) (procedure--references-valid-p item names step)) (rest template))))
   t))

(serapeum/types:-> procedure--input-valid-p (t list) boolean)
(defun procedure--input-valid-p (input names)
  "Return true when INPUT is absent or a list of words and holes binding parameters in NAMES."
  (and (alexandria:proper-list-p input)
       (every
        (lambda (element)
          (or (non-empty-string-p element)
              (and (alexandria:proper-list-p element)
                   (case (first element)
                     (:param
                      (and (= (length element) 4) (member (second element) names :test #'string=)
                           (stringp (third element)) (stringp (fourth element))))
                     (:any
                      (and (= (length element) 3) (stringp (second element))
                           (stringp (third element))))
                     (otherwise
                      nil)))))
        input)
       t))
