(in-package #:autolith)

;;;; -- Running Procedures --

;;; A run calls the procedure's tools one after another through the tool
;;; registry, exactly as if the model had made each call: every call asks for
;;; its own authorization and passes its contracts. Arguments come from the
;;; run's parameters and the earlier calls' results. The first failing call
;;; stops the run, which is recorded in the ledger, and the failure is
;;; reported so the model can continue the work with ordinary calls.

(defparameter *procedure-run-argument-excerpt-limit* 200
  "The most characters of a call's arguments a run report shows.")

(defparameter *procedure-run-output-excerpt-limit* 400
  "The most characters of a call's result a run report shows.")


;;;; -- Public Functions --

(-> procedure-run (tool-context string &key (:parameters list) (:caller keyword)) list)
(defun procedure-run (context identifier &key parameters (caller ':model))
  "Run procedure IDENTIFIER with PARAMETERS, an alist of names and values, under CONTEXT.

A learned procedure that was never admitted is admitted first: by the user's
own (run-procedure) call, or for the model by command authorization. Invalid
parameters, a retired, suspended, or declined procedure, and a call whose
contract requirements do not hold signal KNOWLEDGE-ERROR before any call. Otherwise return a plist of :STATUS (:OK or
:FAILED), :VERSION, :STEPS (each :TOOL, :ARGUMENTS, :SUCCESS-P, :OUTPUT),
:TOTAL, and :FAILURE, after recording the run in the ledger."
  (let* ((configuration (tool-context-configuration context))
         (entry (procedure--runnable-entry context identifier caller))
         (version (procedure-entry-version entry (getf entry :admitted)))
         (procedure (getf version :procedure))
         (bindings (procedure--checked-bindings procedure parameters))
         (steps (getf (rest procedure) :steps))
         (outputs nil)
         (reports nil)
         (failure nil))
    (procedure--check-contracts configuration procedure bindings)
    (loop for step in steps
          for index from 0
          do (let* ((tool-name (getf step :tool))
                    (arguments (handler-case
                                   (procedure-template-instantiate (getf step :arguments)
                                                                   bindings outputs)
                                 (knowledge-error (condition)
                                   (setf failure (format nil "~A could not be prepared: ~A"
                                                         tool-name
                                                         (autolith-error-message condition)))
                                   (loop-finish))))
                    (result (procedure--call context tool-name arguments index)))
               (push (list :tool tool-name
                           :arguments arguments
                           :success-p (tool-result-success-p result)
                           :output (tool-result-content result))
                     reports)
               (unless (tool-result-success-p result)
                 (setf failure (format nil "~A failed: ~A" tool-name
                                       (procedure--excerpt (tool-result-content result)
                                                           *procedure-run-output-excerpt-limit*)))
                 (loop-finish))
               (setf outputs (append outputs
                                     (list (procedure--decode-output
                                            (tool-result-content result)))))))
    (let ((status (if failure ':failed ':ok))
          (completed (count-if (lambda (report) (getf report :success-p)) reports)))
      (procedure-record-run configuration identifier
                            :version (getf version :version)
                            :status status
                            :steps completed
                            :caller caller
                            :failure failure
                            :conversation (and (tool-context-conversation context)
                                               (conversation-identifier
                                                (tool-context-conversation context))))
      (when (and (eq caller ':model) (eq status ':ok)
                 (tool-context-conversation context) (tool-context-call-id context))
        (conversation-append-record
         (tool-context-conversation context)
         (list :procedure-run :call-id (tool-context-call-id context)
               :id identifier :procedure-version (getf version :version)
               :procedure procedure)))
      (list :status status
            :version (getf version :version)
            :steps (nreverse reports)
            :total (length steps)
            :failure failure))))

(-> procedure-run-report (string list) string)
(defun procedure-run-report (identifier run)
  "Return the text reporting RUN of procedure IDENTIFIER: each call with its arguments and result."
  (with-output-to-string (stream)
    (if (eq (getf run :status) ':ok)
        (format stream "Procedure ~A v~D made ~D call~:P:" identifier (getf run :version)
                (length (getf run :steps)))
        (format stream "Procedure ~A v~D stopped after ~D of ~D call~:P: ~A~%Continue the work with ordinary tool calls."
                identifier (getf run :version)
                (count-if (lambda (step) (getf step :success-p)) (getf run :steps))
                (getf run :total) (getf run :failure)))
    (loop for step in (getf run :steps)
          for index from 1
          do (format stream "~%~D. ~A ~A~:[ failed~;~] → ~A"
                     index (getf step :tool)
                     (procedure--excerpt (json-encode (getf step :arguments))
                                         *procedure-run-argument-excerpt-limit*)
                     (getf step :success-p)
                     (procedure--excerpt (getf step :output)
                                         *procedure-run-output-excerpt-limit*)))))

(-> procedure-parameters-from-json (t) list)
(defun procedure-parameters-from-json (object)
  "Return the parameter alist JSON OBJECT names, or signal KNOWLEDGE-ERROR when it is not an object."
  (cond
    ((null object) nil)
    ((json-object-p object)
     (loop for name being the hash-keys of object using (hash-value value)
           collect (cons name value)))
    (t
     (knowledge--fail ':invalid-parameters object "Parameters are an object of names and values."))))


(-> procedure-offers (configuration string) list)
(defun procedure-offers (configuration text)
  "Return the procedures whose request pattern fits request TEXT with every parameter bound.

Each offer is a plist of :ID, :STATUS, :PROCEDURE, and :BINDINGS. Admitted
procedures and never-admitted candidates are offered; suspended and retired
ones, and those whose contract requirements do not hold for these values,
are not."
  (loop for entry in (knowledge-base-procedures (knowledge-base-read configuration)
                                                (knowledge--workspace configuration))
        for status = (procedure-entry-status entry)
        for procedure = (getf (if (eq status ':admitted)
                                  (procedure-entry-runnable-version entry)
                                  (procedure-entry-version entry))
                              :procedure)
        when (member status '(:admitted :candidate))
          append (multiple-value-bind (bindings matched-p) (procedure-match-input procedure text)
                   (and matched-p
                        (every (lambda (parameter)
                                 (assoc (getf parameter :name) bindings :test #'string=))
                               (getf (rest procedure) :parameters))
                        (handler-case
                            (progn (procedure--check-contracts configuration procedure bindings)
                                   t)
                          (knowledge-error ()
                            nil))
                        (list (list :id (getf entry :id)
                                    :status status
                                    :procedure procedure
                                    :bindings (reverse bindings)))))))

(-> procedure-savings (configuration string) (option list))
(defun procedure-savings (configuration identifier)
  "Return measured costs of completed model turns running IDENTIFIER's admitted definition.

The plist holds :TURNS and average :REQUESTS and :TOKENS. Runs without exact
version and definition provenance or complete usage measurements do not count."
  (let* ((entry (knowledge-base-procedure (knowledge-base-read configuration)
                                          (knowledge--workspace configuration) identifier))
         (version (and entry (procedure-entry-runnable-version entry)))
         (traces (and version
                      (action-traces configuration
                                     :predicate (lambda (trace)
                                                  (procedure--ran-p trace identifier version))))))
    (and traces
         (list :turns (length traces)
               :requests (round (reduce #'+ traces :key #'action-trace-requests)
                                (length traces))
               :tokens (round (reduce #'+ traces :key #'action-trace-tokens)
                              (length traces))))))

(-> procedure-automatic-offer (configuration string) (option list))
(defun procedure-automatic-offer (configuration text)
  "Return the one offer for request TEXT that may run before the model reads the request, or NIL.

Running without the model's reading needs more than a fit. The procedure must
be admitted; every word of its request pattern that is not a parameter must
be literal, so a varying word such as \"Please\" or \"Never\" cannot match
anything; and completed model turns running this exact definition must have
used fewer measured requests and tokens than its learning baseline. Anything
less is an offer the model may decline."
  (let ((candidates
          (remove-if-not
           (lambda (offer)
             (and (eq (getf offer :status) ':admitted)
                  (every (lambda (element)
                           (or (stringp element) (eq (first element) :param)))
                         (getf (rest (getf offer :procedure)) :input))
                  (procedure--demonstrated-p configuration (getf offer :id))))
           (procedure-offers configuration text))))
    (and (= 1 (length candidates)) (first candidates))))

(-> procedure-admission-command (string list) string)
(defun procedure-admission-command (identifier version)
  "Return the command text a person approves to admit VERSION of procedure IDENTIFIER."
  (format nil "procedure.admit -- ~A v~D: ~{~A~^, ~}"
          identifier (getf version :version)
          (mapcar (lambda (step) (getf step :tool))
                  (getf (rest (getf version :procedure)) :steps))))


;;;; -- Private Functions --

(-> procedure--runnable-entry (tool-context string keyword) list)
(defun procedure--runnable-entry (context identifier caller)
  "Return IDENTIFIER's entry ready to run, admitting a never-admitted procedure first.

The user's own run admits it; the model's run asks command authorization."
  (let* ((configuration (tool-context-configuration context))
         (workspace (knowledge--workspace configuration))
         (entry (knowledge-base-procedure (knowledge-base-read configuration)
                                          workspace identifier)))
    (unless entry
      (knowledge--fail ':unknown-procedure identifier
                       "No procedure is named ~A; memory:procedures lists them." identifier))
    (ecase (procedure-entry-status entry)
      (:admitted
       entry)
      (:retired
       (knowledge--fail ':procedure-retired identifier
                        "Procedure ~A was retired; it no longer runs." identifier))
      (:suspended
       (knowledge--fail ':procedure-suspended identifier
                        "Procedure ~A is suspended after ~D failed runs in a row, the last: ~A The user can admit it again with (admit-procedure ~S)."
                        identifier (getf entry :consecutive-failures)
                        (or (getf entry :last-failure) "unknown")
                        identifier))
      (:candidate
       (let* ((version (procedure-entry-version entry))
              (decision (if (eq caller ':user)
                            ':full-access
                            (tool-context-authorize-command
                             context
                             (procedure-admission-command identifier version)
                             (config :working-directory configuration)))))
         (when (eq decision ':deny)
           (knowledge--fail ':procedure-declined identifier
                            "Admission of procedure ~A was declined, so it did not run. The user can admit it with (admit-procedure ~S)."
                            identifier identifier))
         (procedure-admit configuration identifier :version (getf version :version))
         (knowledge-base-procedure (knowledge-base-read configuration) workspace identifier))))))

(-> procedure--checked-bindings (list list) list)
(defun procedure--checked-bindings (procedure parameters)
  "Return PARAMETERS as bindings after checking they are exactly PROCEDURE's, each of its type.

Numbers and booleans given as text are read as such first."
  (let ((declared (getf (rest procedure) :parameters))
        (parameters (mapcar (lambda (binding)
                              (let ((parameter (procedure-parameter procedure (first binding))))
                                (cons (first binding)
                                      (if parameter
                                          (procedure--coerce parameter (rest binding))
                                          (rest binding)))))
                            parameters)))
    (dolist (binding parameters)
      (unless (procedure-parameter procedure (first binding))
        (knowledge--fail ':invalid-parameters (first binding)
                         "Procedure ~A takes ~:[no parameters~;~:*~{~A~^, ~}~]; ~A is not one of them."
                         (getf (rest procedure) :id)
                         (mapcar (lambda (parameter) (getf parameter :name)) declared)
                         (first binding))))
    (dolist (parameter declared)
      (let ((binding (assoc (getf parameter :name) parameters :test #'string=)))
        (unless binding
          (knowledge--fail ':invalid-parameters (getf parameter :name)
                           "Procedure ~A needs parameter ~A." (getf (rest procedure) :id)
                           (getf parameter :name)))
        (unless (procedure-parameter-accepts-p parameter (rest binding))
          (knowledge--fail ':invalid-parameters (getf parameter :name)
                           "Parameter ~A must be ~(~A~)~@[ ending in ~A~]."
                           (getf parameter :name)
                           (ecase (getf parameter :type)
                             (:string "a non-empty string")
                             (:integer "an integer")
                             (:number "a number")
                             (:boolean "true or false"))
                           (getf parameter :suffix)))))
    parameters))

(-> procedure--check-contracts (configuration list list) null)
(defun procedure--check-contracts (configuration procedure bindings)
  "Reject grounded blockers before any call; defer only possible earlier-call dependencies.

All requirements must have a joint proof on accepted current facts, applicable
earlier productions, or admitted derivations. Future values stay symbolic and
are checked again against actual results by the ordinary call dispatch."
  (when (knowledge-active-p configuration)
    (let ((view (knowledge-view-current configuration))
          (unknowns (make-hash-table :test #'equal))
          (produced nil))
      (labels ((result-variable (index path)
                 (let ((key (list index path)))
                   (or (gethash key unknowns)
                       (setf (gethash key unknowns) (procedure--preflight-variable))))))
        (loop for step in (getf (rest procedure) :steps)
              for index from 0
              for arguments = (procedure--preflight-arguments
                               (getf step :arguments) bindings #'result-variable)
              for matches = (and (hash-table-p arguments)
                                 (procedure--preflight-matches view (getf step :tool) arguments))
              do (let ((next-productions nil))
                   (dolist (match matches)
                     (destructuring-bind (contract environment) match
                       (let* ((accepted (or (getf (rest contract) :accepts)
                                            *knowledge-established-bases*))
                              (solutions
                                (procedure--preflight-solutions
                                 :view view :patterns (getf (rest contract) :requires)
                                 :environment environment :produced produced :accepted accepted)))
                         (unless solutions
                           (let ((refusal (or (knowledge-contract-refusal configuration (list match) :view view)
                                              "The requirement has no applicable earlier-call proof.")))
                             (knowledge--fail ':procedure-inapplicable (getf (rest procedure) :id)
                                              "Procedure ~A does not apply: call ~D, ~A: ~A"
                                              (getf (rest procedure) :id) (1+ index) (getf step :tool)
                                              (subseq refusal 0
                                                      (or (search " The call was not made." refusal)
                                                          (length refusal))))))
                         (dolist (solution solutions)
                           (setf next-productions
                                 (nconc
                                  (procedure--preflight-productions
                                   :contract contract :environment solution :arguments arguments
                                   :view view :result-variable (lambda (path)
                                                                 (result-variable index path)))
                                  next-productions))))))
                   ;; A call cannot use its own promises, nor another contract on that call.
                   (setf produced (nconc next-productions produced)))))))
  nil)

(-> procedure--call (tool-context string json-object integer) tool-result)
(defun procedure--call (context tool-name arguments index)
  "Make call number INDEX of a run: TOOL-NAME with ARGUMENTS through CONTEXT's registry."
  (let ((tool (find tool-name (tool-registry-tools (tool-context-registry context))
                    :key #'tool-canonical-name :test #'string=)))
    (if (null tool)
        (tool-failure (format nil "Tool ~A is not available here." tool-name))
        (tool-registry-execute-call
         (tool-context-registry context)
         (json-object "type" "function_call"
                      "call_id" (format nil "~@[~A/~]procedure-~D" (tool-context-call-id context)
                                        (1+ index))
                      "namespace" (tool-namespace tool)
                      "name" (tool-name tool)
                      "arguments" (json-encode arguments))
         context))))

(-> procedure--demonstrated-p (configuration string) boolean)
(defun procedure--demonstrated-p (configuration identifier)
  "Return true when completed runs of IDENTIFIER's admitted definition saved requests and tokens."
  (let* ((entry (knowledge-base-procedure (knowledge-base-read configuration)
                                          (knowledge--workspace configuration)
                                          identifier))
         (baseline (getf (procedure-entry-version entry (getf entry :admitted)) :baseline))
         (savings (procedure-savings configuration identifier)))
    (and baseline savings
         (realp (getf baseline :requests)) (realp (getf baseline :tokens))
         (< (getf savings :requests) (getf baseline :requests))
         (< (getf savings :tokens) (getf baseline :tokens))
         t)))

(-> procedure--ran-p (action-trace string list) boolean)
(defun procedure--ran-p (trace identifier version)
  "Return true when TRACE completed a measured model run of IDENTIFIER's exact VERSION."
  (and (action-trace-accepted-p trace)
       (plusp (action-trace-requests trace))
       (action-trace-usage-known-p trace)
       (find-if (lambda (call)
                  (and (equal (getf call :tool) "procedure.run")
                       (eq (getf call :status) ':ok)
                       (json-object-p (getf call :arguments))
                       (equal (json-get (getf call :arguments) "id") identifier)
                       (eql (getf call :procedure-version) (getf version :version))
                       (equal (getf call :procedure) (getf version :procedure))))
                (action-trace-calls trace))
       t))

(-> procedure--coerce (list t) t)
(defun procedure--coerce (parameter value)
  "Return VALUE as PARAMETER's type when it is text spelling one, as typed commands give it."
  (if (stringp value)
      (case (getf parameter :type)
        (:integer
         (handler-case (parse-integer value)
           (error () value)))
        (:number
         (let ((number (ignore-errors (json-decode value))))
           (if (realp number) number value)))
        (:boolean
         (cond
           ((string-equal value "true") t)
           ((string-equal value "false") :json-false)
           (t value)))
        (otherwise
         value))
      value))

(-> procedure--excerpt (t integer) string)
(defun procedure--excerpt (text limit)
  "Return TEXT on one line, cut to LIMIT characters."
  (let ((line (sanitize-text (if (stringp text) text "") :single-line-p t)))
    (if (> (length line) limit)
        (concatenate 'string (subseq line 0 (1- limit)) "…")
        line)))
