(in-package #:autolith)

;;;; -- Running Procedures --

(defparameter *procedure-run-argument-excerpt-limit* 200
  "The most characters of a call's arguments a run report shows.")

(defparameter *procedure-run-output-excerpt-limit* 400
  "The most characters of a call's result a run report shows.")


;;;; -- Tool Calls --

(-> procedure--call
    (tool-context string &key (:arguments t) (:index integer))
    (values tool-result &rest t))
(defun procedure--call (context tool-name &key arguments index)
  "Execute step INDEX with detached native ARGUMENTS through CONTEXT's registry."
  (let ((tool (find tool-name (tool-registry-tools (tool-context-registry context))
                    :key #'tool-canonical-name :test #'string=)))
    (if (null tool)
        (tool-failure (format nil "Tool ~A is not available here." tool-name))
        (tool-registry-execute-native-call
         (tool-context-registry context)
         (json-object "type" "function_call"
                      "namespace" (tool-namespace tool)
                      "name" (tool-name tool)
                      "call_id" (format nil "~@[~A/~]procedure-~D"
                                        (tool-context-call-id context) (1+ index))
                      "arguments" arguments)
         context))))

(-> procedure--result-output (tool-result) t)
(defun procedure--result-output (result)
  "Return the object or array delivered by RESULT, or else its text decoded as JSON or as it is."
  (or (and (tool-result-native-present-p result)
           (let ((value (tool-result-native-value result)))
             (and (or (json-object-p value) (and (vectorp value) (not (stringp value))))
                  value)))
      (procedure--decode-output (tool-result-content result))
      (tool-result-content result)))


;;;; -- Public Functions --

(-> procedure-run (tool-context string &key (:parameters list) (:caller keyword)) list)
(defun procedure-run (context identifier &key parameters (caller ':model))
  "Run procedure IDENTIFIER with PARAMETERS, an alist of names and values, under CONTEXT.

Before executing tools, signal KNOWLEDGE-ERROR for invalid parameters,
inactive, retired or suspended procedures, or unmet contract requirements.
Record the run and return
:STATUS (:OK or :FAILED), :VERSION, :STEPS (each with :TOOL, :ARGUMENTS,
:SUCCESS-P, and :OUTPUT), :TOTAL, and :FAILURE in a plist."
  (let* ((configuration (tool-context-configuration context))
         (entry         (procedure--runnable-entry context identifier))
         (version       (procedure-entry-version entry (getf entry :active)))
         (procedure     (getf version :procedure))
         (bindings      (procedure--checked-bindings procedure parameters))
         (steps         (getf (rest procedure) :steps))
         (outputs       nil)
         (reports       nil)
         (failure       nil))
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
                    (result    (procedure--call context tool-name
                                                    :arguments arguments
                                                    :index     index)))
               (push (list :tool      tool-name
                           :arguments arguments
                           :success-p (tool-result-success-p result)
                           :output    (tool-result-content result))
                     reports)
               (unless (tool-result-success-p result)
                 (setf failure (format nil "~A failed: ~A" tool-name
                                       (procedure--excerpt (tool-result-content result)
                                                           *procedure-run-output-excerpt-limit*)))
                 (loop-finish))
               (setf outputs (append outputs (list (procedure--result-output result))))))
    (let ((status    (if failure ':failed ':ok))
          (completed (count-if (lambda (report) (getf report :success-p)) reports)))
      (procedure-record-run configuration identifier
                            :version      (getf version :version)
                            :status       status
                            :steps        completed
                            :caller       caller
                            :failure      failure
                            :conversation (and (tool-context-conversation context)
                                               (conversation-identifier
                                                (tool-context-conversation context))))
      (when (and (eq caller ':model) (eq status ':ok)
                 (tool-context-conversation context) (tool-context-call-id context))
        (conversation-append-record
         (tool-context-conversation context)
         (list :procedure-run :call-id (tool-context-call-id context)
               :id identifier :procedure-version (getf version :version)
               :procedure     procedure)))
      (list :status  status
            :version (getf version :version)
            :steps   (nreverse reports)
            :total   (length steps)
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
    ((null object)
     nil)
    ((json-object-p object)
     (loop for name being the hash-keys of object using (hash-value value)
           collect (cons name value)))
    (t
     (knowledge--fail ':invalid-parameters object "Parameters are an object of names and values."))))

(-> procedure-savings (configuration string) (option list))
(defun procedure-savings (configuration identifier)
  "Return measured costs of completed model turns running IDENTIFIER's active definition.

The plist holds :TURNS and average :REQUESTS and :TOKENS. Runs without exact
version and definition provenance or complete usage measurements do not count."
  (let* ((entry   (knowledge-base-procedure (knowledge-base-read configuration)
                                            (knowledge--workspace configuration) identifier))
         (version (and entry (procedure-entry-runnable-version entry)))
         (runs    (and version
                       (action-trace-procedure-measurements
                        configuration identifier :version   (getf version :version)
                                                 :procedure (getf version :procedure)))))
    (and runs
         (list :turns    (length runs)
               :requests (round (reduce #'+ runs :key #'first) (length runs))
               :tokens   (round (reduce #'+ runs :key #'second) (length runs))))))


;;;; -- Private Functions --

(-> procedure--runnable-entry (tool-context string) list)
(defun procedure--runnable-entry (context identifier)
  "Return IDENTIFIER's active entry, or explain why it cannot run."
  (let* ((configuration (tool-context-configuration context))
         (workspace     (knowledge--workspace configuration))
         (entry         (knowledge-base-procedure (knowledge-base-read configuration)
                                                  workspace identifier)))
    (unless entry
      (knowledge--fail ':unknown-procedure identifier
                       "No procedure is named ~A; memory:procedures lists them." identifier))
    (ecase (procedure-entry-status entry)
      (:active
       entry)
      (:retired
       (knowledge--fail ':procedure-retired identifier
                        "Procedure ~A was retired; it no longer runs." identifier))
      (:suspended
       (knowledge--fail ':procedure-suspended identifier
                        "Procedure ~A is suspended after ~D failed runs in a row, the last: ~A Correct its definition before running it again."
                        identifier (getf entry :consecutive-failures)
                        (or (getf entry :last-failure) "unknown")))
      (:inactive
       (knowledge--fail ':procedure-inactive identifier
                        "Legacy procedure ~A is inactive; define it again before running it."
                        identifier)))))

(-> procedure--checked-bindings (list list) list)
(defun procedure--checked-bindings (procedure parameters)
  "Return PARAMETERS as bindings after checking they are exactly PROCEDURE's, each of its type.

Numbers and booleans given as text are read as such first."
  (let ((declared   (getf (rest procedure) :parameters))
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
                             (:string
                              "a non-empty string")
                             (:integer
                              "an integer")
                             (:number
                              "a number")
                             (:boolean
                              "true or false"))
                           (getf parameter :suffix)))))
    parameters))

(-> procedure--check-contracts (configuration list list) null)
(defun procedure--check-contracts (configuration procedure bindings)
  "Reject grounded blockers before any call; defer only possible earlier-call dependencies.

All requirements must have a joint proof on accepted current facts, applicable
earlier productions, or active rules' derivations. Future values stay symbolic and
are checked again against actual results by the ordinary call dispatch."
  (when (knowledge-active-p configuration)
    (let ((view     (knowledge-view-current configuration))
          (unknowns (make-hash-table :test #'equal))
          (produced nil))
      (labels ((result-variable (index path)
                 (let ((key (list index path)))
                   (values (ensure-gethash key unknowns
                                           (procedure--preflight-variable))))))
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

(-> procedure--coerce (list t) t)
(defun procedure--coerce (parameter value)
  "Return VALUE as PARAMETER's type when it is text spelling one, as typed commands give it."
  (if (stringp value)
      (case (getf parameter :type)
        (:integer
         (handler-case (parse-integer value)
           (error () value)))
        (:number
         (let ((number (handler-case (json-decode value)
                         (json-error ()
                           nil))))
           (if (realp number) number value)))
        (:boolean
         (cond
           ((string-equal value "true")
            t)
           ((string-equal value "false")
            :json-false)
           (t
            value)))
        (otherwise
         value))
      value))

(-> procedure--excerpt (t integer) string)
(defun procedure--excerpt (text limit)
  "Return TEXT on one line, cut to LIMIT characters."
  (let ((line (sanitize-text (if (stringp text) text "") :single-line-p t)))
    (text--bounded-ellipsis line limit :ellipsis "…" :prefix-limit (1- limit))))
