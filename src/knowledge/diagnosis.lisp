(in-package #:autolith)

;;;; -- Diagnosis --

(defparameter *knowledge-diagnosis-hypothesis-limit* 4
  "Maximum hypotheses per diagnosis.")

(defparameter *knowledge-diagnosis-probe-limit* 8
  "Maximum probes per diagnosis.")

(defparameter *knowledge-diagnosis-run-limit* 3
  "Maximum probes per case.")


;;;; -- Validation --

(-> knowledge-diagnosis-p (t) boolean)
(defun knowledge-diagnosis-p (diagnosis)
  "Return true when DIAGNOSIS has the structure of a validated diagnosis."
  (and (proper-list-p diagnosis)
       (eq (first diagnosis) :diagnosis)
       (evenp (length (rest diagnosis)))
       (let ((properties (rest diagnosis)))
         (and (knowledge-rule-name-p (getf properties :id))
              (every #'knowledge-variable-p (getf properties :parameters))
              (<= 1 (length (getf properties :hypotheses)) *knowledge-diagnosis-hypothesis-limit*)
              (every (lambda (hypothesis)
                       (and (proper-list-p hypothesis)
                            (knowledge-rule-name-p (getf hypothesis :id))
                            (getf hypothesis :predicts)
                            (every #'consp (getf hypothesis :predicts))
                            (or (null (getf hypothesis :repair))
                                (stringp (getf hypothesis :repair)))))
                     (getf properties :hypotheses))
              (<= (length (getf properties :probes)) *knowledge-diagnosis-probe-limit*)
              (every (lambda (probe)
                       (and (proper-list-p probe)
                            (non-empty-string-p (getf probe :tool))
                            (every (lambda (argument)
                                     (and (proper-list-p argument)
                                          (= (length argument) 2)
                                          (stringp (first argument))))
                                   (getf probe :arguments))
                            (realp (getf probe :cost))
                            (plusp (getf probe :cost))))
                     (getf properties :probes))))
       t))

(-> knowledge-make-diagnosis
    (&key (:id string) (:parameters sequence) (:hypotheses sequence) (:probes sequence)
          (:description (option string)) (:schema-function function))
    list)
(defun knowledge-make-diagnosis
    (&key id parameters hypotheses probes description schema-function)
  "Return a validated diagnosis built from input forms.

HYPOTHESES are objects with id, predicts (fact patterns, negatable), and an
optional repair; PROBES are objects with tool, arguments, and an optional
positive cost. Every variable must be a parameter."
  (unless (knowledge-rule-name-p id)
    (knowledge--fail ':invalid-diagnosis id
                     "Diagnosis names are lowercase letters, digits, hyphens, and dots."))
  (let ((parameters (coerce (or parameters #()) 'list))
        (hypotheses (coerce (or hypotheses #()) 'list))
        (probes     (coerce (or probes #()) 'list)))
    (unless (every #'knowledge-variable-p parameters)
      (knowledge--fail ':invalid-diagnosis parameters "Parameters are variables such as ?service."))
    (unless (<= 1 (length hypotheses) *knowledge-diagnosis-hypothesis-limit*)
      (knowledge--fail ':invalid-diagnosis id "A diagnosis weighs 1 to ~D hypotheses."
                       *knowledge-diagnosis-hypothesis-limit*))
    (unless (<= (length probes) *knowledge-diagnosis-probe-limit*)
      (knowledge--fail ':invalid-diagnosis id "A diagnosis lists at most ~D probes."
                       *knowledge-diagnosis-probe-limit*))
    (flet ((check-variables (value)
             (when (and (knowledge-variable-p value)
                        (not (member value parameters :test #'string=)))
               (knowledge--fail ':invalid-diagnosis value "~A is not a parameter of ~A." value id))
             value))
      (let ((diagnosis
              (list :diagnosis
                    :id id
                    :parameters  parameters
                    :hypotheses
                    (mapcar
                     (lambda (hypothesis)
                       (unless (hash-table-p hypothesis)
                         (knowledge--fail ':invalid-diagnosis hypothesis
                                          "Each hypothesis is an object with id, predicts, and an optional repair."))
                       (let ((predictions
                               (mapcar (lambda (pattern)
                                         (let ((parsed (knowledge--pattern-from-input
                                                        pattern schema-function :negatable t)))
                                           (mapc #'check-variables
                                                 (rest (knowledge-rule-premise-atom parsed)))
                                           parsed))
                                       (coerce (or (gethash "predicts" hypothesis) #()) 'list))))
                         (unless predictions
                           (knowledge--fail ':invalid-diagnosis (gethash "id" hypothesis)
                                            "Hypothesis ~A predicts nothing a probe could contradict."
                                            (gethash "id" hypothesis)))
                         (append (list :id (gethash "id" hypothesis) :predicts predictions)
                                 (when (gethash "description" hypothesis)
                                   (list :description (gethash "description" hypothesis)))
                                 (when (gethash "repair" hypothesis)
                                   (list :repair (gethash "repair" hypothesis))))))
                     hypotheses)
                    :probes
                    (mapcar
                     (lambda (probe)
                       (unless (hash-table-p probe)
                         (knowledge--fail ':invalid-diagnosis probe
                                          "Each probe is an object with tool, arguments, and an optional cost."))
                       (let ((arguments (or (gethash "arguments" probe) (json-object)))
                             (cost      (or (gethash "cost" probe) 1)))
                         (unless (json-object-p arguments)
                           (knowledge--fail ':invalid-diagnosis probe
                                            "A probe's arguments are an object."))
                         (unless (and (realp cost) (plusp cost))
                           (knowledge--fail ':invalid-diagnosis probe "A probe's cost is positive."))
                         (list :tool      (gethash "tool" probe)
                               :arguments (loop for name being the hash-keys of arguments
                                                  using (hash-value value)
                                                collect (list name (check-variables value)))
                               :cost      (rationalize cost))))
                     probes)
                    :description description)))
        (unless (knowledge-diagnosis-p diagnosis)
          (knowledge--fail ':invalid-diagnosis id "The diagnosis is malformed."))
        (let ((names (mapcar (lambda (hypothesis) (getf hypothesis :id))
                             (getf (rest diagnosis) :hypotheses))))
          (unless (= (length names) (length (remove-duplicates names :test #'string=)))
            (knowledge--fail ':invalid-diagnosis id "Hypothesis names must differ.")))
        diagnosis))))


;;;; -- Stored Diagnoses and Cases --

(-> knowledge-base-diagnosis (knowledge-base string &key (:workspace (option string)))
    (option list))
(defun knowledge-base-diagnosis (base identifier &key workspace)
  "Return diagnosis IDENTIFIER visible from WORKSPACE, a workspace one before a global one."
  (let ((entry (or (gethash (list ':workspace workspace identifier)
                            (knowledge-base-diagnoses-table base))
                   (gethash (list ':global nil identifier)
                            (knowledge-base-diagnoses-table base)))))
    (and entry (getf entry :diagnosis))))

(-> knowledge-base-diagnoses (knowledge-base &key (:workspace (option string))) list)
(defun knowledge-base-diagnoses (base &key workspace)
  "Return the diagnoses visible from WORKSPACE, ordered by name."
  (sort (loop for entry being the hash-values of (knowledge-base-diagnoses-table base)
              when (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
                collect (getf entry :diagnosis))
        #'string< :key (lambda (diagnosis) (getf (rest diagnosis) :id))))

(-> knowledge-base-diagnosis-cases (knowledge-base string) list)
(defun knowledge-base-diagnosis-cases (base workspace)
  "Return WORKSPACE's recent diagnosis cases, newest first."
  (gethash workspace (knowledge-base-diagnosis-cases-table base)))

(-> knowledge-define-diagnosis
    (configuration &key (:id string) (:parameters sequence) (:hypotheses sequence)
                        (:probes sequence) (:description (option string)) (:scope memory-scope)
                        (:source-conversation (option string)))
    (values string boolean))
(defun knowledge-define-diagnosis
    (configuration &key id parameters hypotheses probes description (scope ':workspace)
                        source-conversation)
  "Validate and record diagnosis ID, replacing an earlier one of that name in SCOPE.

Return its ID and whether the definition changed."
  (let ((workspace (knowledge--owner configuration scope))
        (viewer    (knowledge--workspace configuration)))
    (values-list
     (knowledge-transact
      configuration
      (lambda (base)
        (let* ((diagnosis (knowledge-make-diagnosis
                           :id id :parameters parameters :hypotheses hypotheses :probes probes
                           :description description
                           :schema-function
                           (lambda (predicate)
                             (knowledge-base-schema base predicate
                                                    :workspace (and (eq scope ':workspace)
                                                                    viewer)))))
               (existing  (gethash (list scope workspace id)
                                   (knowledge-base-diagnoses-table base))))
          (if (equal (getf existing :diagnosis) diagnosis)
              (values nil (list id nil) nil)
              (values (list (list :diagnosis-defined
                                  :version *knowledge-format-version*
                                  :scope               scope
                                  :workspace           workspace
                                  :diagnosis           diagnosis
                                  :time                (get-universal-time)
                                  :source-conversation source-conversation))
                      (list id t)
                      t))))))))


;;;; -- Predictions --

(-> knowledge-prediction-status (knowledge-view list) (values keyword (option knowledge-node)))
(defun knowledge-prediction-status (view prediction)
  "Return whether ground PREDICTION is :CONFIRMED, :CONTRADICTED, or :UNKNOWN in VIEW.

Return the deciding fact as the second value. Use only undisputed observations
or user statements, excluding predictions, assumptions, and conflicted facts.
A positive prediction is contradicted by an explicit negation or a different
value for the same predicate key. A negated prediction is contradicted by the
fact it denies. Missing facts do not contradict predictions."
  (if (knowledge-negation-p prediction)
      (let ((denied    (first (knowledge--established view (second prediction))))
            (confirmed (first (knowledge--established view prediction))))
        (cond
          (denied
           (values ':contradicted denied))
          (confirmed
           (values ':confirmed confirmed))
          (t
           (values ':unknown nil))))
      (let ((confirmed (first (knowledge--established view prediction)))
            (negated   (first (knowledge--established view (list :not prediction))))
            (rival     (let ((pattern (knowledge--observation-key view prediction)))
                         (and pattern
                              (find-if-not (lambda (node)
                                             (equal (knowledge-node-proposition node)
                                                    prediction))
                                           (knowledge--established view pattern))))))
        (cond
          (confirmed
           (values ':confirmed confirmed))
          (negated
           (values ':contradicted negated))
          (rival
           (values ':contradicted rival))
          (t
           (values ':unknown nil))))))

(-> knowledge--established (knowledge-view list) list)
(defun knowledge--established (view pattern)
  "Return VIEW's admissible facts matching PATTERN."
  (remove-if-not (lambda (node) (knowledge-node-admissible-p view node))
                 (knowledge-view-holds view pattern)))


;;;; -- Running a Case --

(-> knowledge-diagnosis-instantiate (list list function) list)
(defun knowledge-diagnosis-instantiate (diagnosis bindings schema-function)
  "Return DIAGNOSIS with its parameters replaced by BINDINGS, an alist of variables and values.

Every parameter must be bound. In predictions a value is read by its
predicate's schema, found with SCHEMA-FUNCTION; probe arguments take it as given."
  (let ((environment
          (mapcar (lambda (parameter)
                    (let ((binding (assoc parameter bindings :test #'string=)))
                      (unless binding
                        (knowledge--fail ':invalid-diagnosis parameter
                                         "Diagnosis ~A needs a value for ~A."
                                         (getf (rest diagnosis) :id) parameter))
                      binding))
                  (getf (rest diagnosis) :parameters))))
    (labels ((prediction (pattern)
               (if (knowledge-negation-p pattern)
                   (list :not (prediction (second pattern)))
                   (let ((schema (funcall schema-function (first pattern))))
                     (cons (first pattern)
                           (loop for item in (rest pattern)
                                 for argument in (knowledge-schema-arguments schema)
                                 collect (let ((binding (and (knowledge-variable-p item)
                                                             (assoc item environment
                                                                    :test #'string=))))
                                           (if binding
                                               (knowledge-term-from-input (rest binding)
                                                                          (getf argument :type))
                                               item))))))))
      (list* :diagnosis
             :hypotheses
             (mapcar (lambda (hypothesis)
                       (let ((copy (copy-list hypothesis)))
                         (setf (getf copy :predicts)
                               (mapcar #'prediction (getf hypothesis :predicts)))
                         copy))
                     (getf (rest diagnosis) :hypotheses))
             :probes
             (mapcar (lambda (probe)
                       (list :tool      (getf probe :tool)
                             :arguments (loop for (name item) in (getf probe :arguments)
                                              collect (list name (knowledge--instantiate-term
                                                                  item environment)))
                             :cost      (getf probe :cost)))
                     (getf (rest diagnosis) :probes))
             :bindings environment
             (let ((properties (copy-list (rest diagnosis))))
               (remf properties :hypotheses)
               (remf properties :probes)
               (remf properties :parameters)
               properties)))))

(-> knowledge-diagnosis-run (tool-context list &key (:bindings list)) list)
(defun knowledge-diagnosis-run (context diagnosis &key bindings)
  "Run DIAGNOSIS with parameter BINDINGS under CONTEXT and record the case.

Return the case: :REMAINING hypothesis plists, :ELIMINATED as (:HYPOTHESIS
:BY :TEXT), :PROBES run as (:TOOL :ARGUMENTS :STATUS :OBSERVED), :REPAIR when
one hypothesis remains and names one, and :OPEN when several remain."
  (let* ((configuration (tool-context-configuration context))
         (base          (knowledge-base-read configuration))
         (workspace     (knowledge--workspace configuration))
         (instance      (knowledge-diagnosis-instantiate
                         diagnosis bindings
                         (lambda (predicate)
                           (or (knowledge-base-schema base predicate :workspace workspace)
                               (knowledge--fail ':unknown-predicate predicate
                                                "Define predicate ~A before predicting it." predicate)))))
         (hypotheses    (getf (rest instance) :hypotheses))
         (available     (getf (rest instance) :probes))
         (eliminated    nil)
         (ran           nil))
    (flet ((weigh ()
             ;; Eliminate every remaining hypothesis a current fact contradicts.
             (let ((view (knowledge-view-current configuration)))
               (setf hypotheses
                     (remove-if
                      (lambda (hypothesis)
                        (loop for prediction in (getf hypothesis :predicts)
                              do (multiple-value-bind (status node)
                                     (knowledge-prediction-status view prediction)
                                   (when (eq status ':contradicted)
                                     (push (list :hypothesis (getf hypothesis :id)
                                                 :by         (knowledge-node-identifier node)
                                                 :text       (knowledge-node-text node))
                                           eliminated)
                                     (return t)))))
                      hypotheses))
               view)))
      (loop for view = (weigh)
            while (and (rest hypotheses) (< (length ran) *knowledge-diagnosis-run-limit*))
            do (let ((probe (knowledge--best-probe context view
                                                   :hypotheses hypotheses
                                                   :probes     available)))
                 (unless probe
                   (loop-finish))
                 (setf available (remove probe available))
                 (push (knowledge--run-probe context probe) ran))))
    (let* ((remaining hypotheses)
           (case      (list :diagnosis  (getf (rest diagnosis) :id)
                            :bindings   (getf (rest instance) :bindings)
                            :remaining  (mapcar (lambda (hypothesis) (getf hypothesis :id)) remaining)
                            :eliminated (reverse eliminated)
                            :probes     (reverse ran)
                            :repair     (and (= 1 (length remaining)) (getf (first remaining) :repair))
                            :open       (and (rest remaining) t))))
      (knowledge--record-case configuration case
                              :conversation (and (tool-context-conversation context)
                                                 (conversation-identifier
                                                  (tool-context-conversation context))))
      case)))

(-> knowledge-diagnosis-report (list) string)
(defun knowledge-diagnosis-report (case)
  "Return the text reporting CASE: the probes run, what they eliminated, and what remains."
  (with-output-to-string (stream)
    (let ((remaining (getf case :remaining)))
      (cond
        ((null remaining)
         (format stream "Diagnosis ~A: every hypothesis was contradicted; the cause is something else."
                 (getf case :diagnosis)))
        ((rest remaining)
         (format stream "Diagnosis ~A is still open: ~{~A~^, ~} remain~:[~; and no listed probe tells them apart~]."
                 (getf case :diagnosis) remaining
                 (< (length (getf case :probes)) *knowledge-diagnosis-run-limit*)))
        (t
         (format stream "Diagnosis ~A: ~A~@[. Repair: ~A~]"
                 (getf case :diagnosis) (first remaining) (getf case :repair)))))
    (loop for probe in (getf case :probes)
          for index from 1
          do (format stream "~%probe ~D: ~A ~A ~(~A~)~@[ → ~{~A~^; ~}~]"
                     index (getf probe :tool) (json-encode (getf probe :arguments-json))
                     (getf probe :status) (getf probe :observed)))
    (dolist (entry (getf case :eliminated))
      (format stream "~%ruled out ~A: ~A (fact ~A)"
              (getf entry :hypothesis) (getf entry :text) (getf entry :by)))))


;;;; -- Private Functions --

(-> knowledge--probe-contracts (tool-context list) list)
(defun knowledge--probe-contracts (context probe)
  "Return contract matches for PROBE's resolved tool, or NIL if unavailable."
  (let* ((registry (tool-context-registry context))
         (tool     (tool-registry-resolve registry (getf probe :tool))))
    (when tool
      (tool-contracts-for-call (tool-context-configuration context) tool
                              (knowledge--probe-arguments probe) :registry registry))))

(-> knowledge--probe-observations (tool-context list) list)
(defun knowledge--probe-observations (context probe)
  "Return (PREDICATE VALUE...) for each fact PROBE's contracts observe.

Values known before the call are read by the predicate's schema; values the
result will supply are NIL."
  (let* ((configuration (tool-context-configuration context))
         (arguments     (knowledge--probe-arguments probe))
         (base          (knowledge-base-read configuration))
         (workspace     (knowledge--workspace configuration)))
    (loop for (contract environment) in (knowledge--probe-contracts context probe)
          append (loop for observation in (getf (rest contract) :observes)
                       for schema = (knowledge-base-schema base (getf observation :predicate)
                                                           :workspace workspace)
                       when schema
                         collect (cons (getf observation :predicate)
                                       (loop for argument in (getf observation :arguments)
                                             for definition in (knowledge-schema-arguments schema)
                                             for value = (cond
                                                            ((and (stringp argument)
                                                                  (uiop:string-prefix-p "$arg." argument))
                                                             (values (knowledge--reference-value
                                                                      argument arguments nil)))
                                                           ((knowledge--reference-p argument)
                                                            nil)
                                                           ((knowledge-variable-p argument)
                                                            (rest (assoc argument environment
                                                                         :test #'string=)))
                                                           (t
                                                            argument))
                                             collect (and value
                                                          (handler-case
                                                              (knowledge-term-from-input
                                                               value (getf definition :type))
                                                            (knowledge-error ()
                                                              nil)))))))))

(-> knowledge--probe-arguments (list) hash-table)
(defun knowledge--probe-arguments (probe)
  "Return PROBE's call arguments as a JSON object."
  (let ((arguments (json-object)))
    (loop for (name value) in (getf probe :arguments)
          do (setf (gethash name arguments)
                   (cond
                     ((eq value ':true)
                      t)
                     ((eq value ':false)
                      (json-false))
                     (t
                      value))))
    arguments))

(-> knowledge--observation-key (knowledge-view list) (option list))
(defun knowledge--observation-key (view observation)
  "Return the pattern of the fact OBSERVATION identifies, or NIL when its key is not known.

The key arguments are OBSERVATION's; every other argument is a variable."
  (let* ((schema (knowledge-base-schema (knowledge-view-base view) (first observation)
                                        :workspace (knowledge-view-workspace view)))
         (key    (and schema (knowledge-schema-key schema))))
    (and key
         (every (lambda (position) (nth (1+ position) observation)) key)
         (cons (first observation)
               (loop for value in (rest observation)
                     for position from 0
                     collect (if (member position key)
                                 value
                                 (format nil "?value~D" position)))))))

(-> knowledge--prediction-about (list list) (option list))
(defun knowledge--prediction-about (hypothesis key-pattern)
  "Return HYPOTHESIS's prediction about the fact KEY-PATTERN identifies, or NIL."
  (find-if (lambda (prediction)
             (let ((atom (knowledge-rule-premise-atom prediction)))
               (and (string= (first atom) (first key-pattern))
                    (nth-value 1 (knowledge--unify key-pattern atom nil)))))
           (getf hypothesis :predicts)))

(-> knowledge--pair-separation (list list list) rational)
(defun knowledge--pair-separation (left right key-pattern)
  "Return a probe-ranking weight for LEFT and RIGHT at KEY-PATTERN.

Use 1 when both hypotheses have different prediction forms, 1/4 when only
one has a prediction, and 0 when their predictions are equal or both absent.
These weights rank probes; they are not outcome probabilities."
  (let ((left-prediction  (knowledge--prediction-about left key-pattern))
        (right-prediction (knowledge--prediction-about right key-pattern)))
    (cond
      ((and left-prediction right-prediction)
       (if (equal left-prediction right-prediction) 0 1))
      ((or left-prediction right-prediction)
       1/4)
      (t
       0))))

(-> knowledge--probe-admissible-p (tool-context list) boolean)
(defun knowledge--probe-admissible-p (context probe)
  "Return true when PROBE's contracted requirements permit its call now."
  (handler-case
      (not (knowledge-contract-refusal
            (tool-context-configuration context)
            (knowledge--probe-contracts context probe)))
    (knowledge-error ()
      nil)))

(-> knowledge--best-probe
    (tool-context knowledge-view &key (:hypotheses list) (:probes list)) (option list))
(defun knowledge--best-probe (context view &key hypotheses probes)
  "Return the eligible probe with the highest positive score per unit cost, or NIL.

For each hypothesis pair, use its largest `knowledge--pair-separation' weight
across the probe's unknown observations. Sum these weights and divide by cost.
Skip unavailable tools, known observations, and calls refused by contracts."
  (let ((best       nil)
        (best-score 0))
    (dolist (probe (remove-if-not (lambda (candidate)
                                   (knowledge--probe-admissible-p context candidate))
                                probes))
      (let* ((keys  (loop for observation in (handler-case
                                                 (knowledge--probe-observations context probe)
                                               (knowledge-error ()
                                                 nil))
                          for key = (knowledge--observation-key view observation)
                          when (and key (null (knowledge--established view key)))
                            collect key))
             (pairs (loop for (left . others) on hypotheses
                          sum (loop for right in others
                                    sum (loop for key in keys
                                              maximize (knowledge--pair-separation left right key)))))
             (score (/ pairs (getf probe :cost))))
        (when (> score best-score)
          (setf best probe
                best-score score))))
    best))

(-> knowledge--run-probe (tool-context list) list)
(defun knowledge--run-probe (context probe)
  "Run PROBE through CONTEXT's tool registry and return what it observed."
  (let* ((arguments     (knowledge--probe-arguments probe))
         (tool          (tool-registry-resolve (tool-context-registry context) (getf probe :tool)))
         (result        (if tool
                            (tool-registry-execute-call
                             (tool-context-registry context)
                             (json-object "type" "function_call"
                                          "call_id" (format nil "~@[~A/~]probe" (tool-context-call-id context))
                                          "namespace" (tool-namespace tool)
                                          "name" (tool-name tool)
                                          "arguments" (json-encode arguments))
                             context)
                            (tool-failure (format nil "Tool ~A is unavailable or its display name is ambiguous."
                                                  (getf probe :tool)))))
         (configuration (tool-context-configuration context))
         (view          (knowledge-view-current configuration)))
    (list :tool           (getf probe :tool)
          :arguments-json arguments
          :status         (if (tool-result-success-p result) ':ok ':failed)
          :observed       (and (tool-result-success-p result)
                               (loop for observation in (handler-case
                                                            (knowledge--probe-observations context probe)
                                                          (knowledge-error ()
                                                            nil))
                                     for key = (knowledge--observation-key view observation)
                                     append (mapcar #'knowledge-node-text
                                                    (and key (knowledge--established view key))))))))

(-> knowledge--record-case (configuration list &key (:conversation (option string))) null)
(defun knowledge--record-case (configuration case &key conversation)
  "Append diagnosis CASE, with the facts that decided it, to the knowledge log."
  (let ((workspace (knowledge--workspace configuration)))
    (knowledge-transact
     configuration
     (lambda (base)
       (declare (ignore base))
       (values (list (list :diagnosis-recorded
                           :scope ':workspace
                           :workspace    workspace
                           :case         (list :diagnosis  (getf case :diagnosis)
                                               :bindings   (mapcar (lambda (binding)
                                                                     (list (first binding) (rest binding)))
                                                                   (getf case :bindings))
                                               :remaining  (getf case :remaining)
                                               :eliminated (getf case :eliminated)
                                               :probes     (mapcar (lambda (probe)
                                                                     (list :tool     (getf probe :tool)
                                                                           :status   (getf probe :status)
                                                                           :observed (getf probe :observed)))
                                                                   (getf case :probes))
                                               :repair     (getf case :repair))
                           :conversation conversation
                           :time         (get-universal-time)))
               nil
               t))))
  nil)
