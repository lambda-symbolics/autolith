(in-package #:autolith)

;;;; -- Diagnosis Tests --

(defparameter *diagnosis-tests-repairs*
  '(("unavailable" . "service.restart")
    ("stale-endpoint" . "service.update-endpoint")
    ("credentials" . "service.rotate-credentials"))
  "The repair tool each remote-service cause calls for.")

(defclass diagnosis-tests--world ()
  ((fault
    :initarg       :fault
    :accessor      diagnosis-tests--world-fault
    :documentation "What is wrong: unavailable, stale-endpoint, credentials, or NIL.")
   (registry
    :initarg       :registry
    :reader        diagnosis-tests--world-registry
    :documentation "The registry holding the service tools.")
   (repairs
    :initform      nil
    :accessor      diagnosis-tests--world-repairs
    :documentation "The repairs tried, newest first."))
  (:documentation "A remote service with one fault and the tools that probe and repair it."))

(defclass diagnosis-tests--wire-named-tool (learning-tests--scripted-tool)
  ((display
    :initarg       :display
    :reader        diagnosis-tests--wire-named-tool-display
    :type          string
    :documentation "The tool's display alias."))
  (:documentation "A scripted tool with separate canonical and display names."))

(defmethod tool-display-name ((tool diagnosis-tests--wire-named-tool))
  "Return the scripted display name."
  (diagnosis-tests--wire-named-tool-display tool))

(-> diagnosis-tests--wire-named-tool (tool-registry string function) tool)
(defun diagnosis-tests--wire-named-tool (registry tool respond)
  "Register a scripted tool with TOOL as its display alias."
  (let* ((dot      (position #\. tool))
         (instance (make-instance 'diagnosis-tests--wire-named-tool
                                  :namespace   (format nil "mcp__~A_0a1b2c" (subseq tool 0 dot))
                                  :name        (format nil "~A_3d4e5f" (subseq tool (1+ dot)))
                                  :display     tool
                                  :description "A scripted tool with a wire name."
                                  :parameters  (json-object "type" "object" "additionalProperties" t)
                                  :respond     respond)))
    (tool-registry-register registry instance)
    instance))

(-> diagnosis-tests--world-create
    (configuration (option string) &key (:config-p boolean) (:wire-names-p boolean))
    diagnosis-tests--world)
(defun diagnosis-tests--world-create (configuration fault &key (config-p t) wire-names-p)
  "Return a world under CONFIGURATION whose service has FAULT; without CONFIG-P its endpoint cannot be read.

With WIRE-NAMES-P, probe tools have separate canonical and display names."
  (let* ((registry (make-default-tool-registry :configuration configuration))
         (world    (make-instance 'diagnosis-tests--world :fault fault :registry registry)))
    (flet ((fault-p (name)
             (equal (diagnosis-tests--world-fault world) name))
           (learning-tests--tool (registry tool respond)
             (if wire-names-p
                 (diagnosis-tests--wire-named-tool registry tool respond)
                 (learning-tests--tool registry tool respond))))
      (learning-tests--tool registry "service.ping"
                            (lambda (arguments)
                              (declare (ignore arguments))
                              (json-object "reachable" (if (fault-p "unavailable") (json-false) t))))
      (when config-p
        (learning-tests--tool registry "service.config"
                              (lambda (arguments)
                                (declare (ignore arguments))
                                (json-object "current" (if (fault-p "stale-endpoint") (json-false) t)))))
      (learning-tests--tool registry "service.auth"
                            (lambda (arguments)
                              (declare (ignore arguments))
                              (json-object "accepted" (if (fault-p "credentials") (json-false) t))))
      (learning-tests--tool registry "service.check"
                            (lambda (arguments)
                              (declare (ignore arguments))
                              (json-object "healthy" (if (diagnosis-tests--world-fault world)
                                                         (json-false)
                                                         t))))
      (loop for (cause . tool) in *diagnosis-tests-repairs*
            do (let ((cause cause)
                     (tool  tool))
                 (learning-tests--tool registry tool
                                       (lambda (arguments)
                                         (declare (ignore arguments))
                                         (push tool (diagnosis-tests--world-repairs world))
                                         (when (fault-p cause)
                                           (setf (diagnosis-tests--world-fault world) nil))
                                         (json-object "done" t))))))
    world))

(-> diagnosis-tests--define-vocabulary (configuration) null)
(defun diagnosis-tests--define-vocabulary (configuration)
  "Define the service predicates and the contracts turning probe results into facts."
  (loop for (predicate value) in '(("service.reachable" "reachable")
                                   ("endpoint.current" "current")
                                   ("auth.accepted" "accepted"))
        do (knowledge-define-schema
            configuration
            (knowledge-make-schema :predicate predicate
                                   :arguments `((:name "service" :type :entity)
                                                (:name ,value :type :boolean))
                                   :key       '("service"))))
  (loop for (tool predicate field) in '(("service.ping" "service.reachable" "reachable")
                                        ("service.config" "endpoint.current" "current")
                                        ("service.auth" "auth.accepted" "accepted"))
        do (knowledge-define-contract
            configuration
            :id (substitute #\- #\. tool) :tool tool
            :when     (json-object "service" "?service")
            :observes (vector (json-object "predicate" predicate
                                           "arguments" (vector "$arg.service"
                                                               (format nil "$result.~A" field))))))
  nil)

(-> diagnosis-tests--definition () json-object)
(defun diagnosis-tests--definition ()
  "Return the remote-service diagnosis as diagnosis.run arguments that save it."
  (json-decode "{\"save_as\": \"remote-service\", \"parameters\": [\"service\"],
  \"bindings\": {\"service\": \"billing\"},
  \"hypotheses\": [
    {\"id\": \"unavailable\", \"predicts\": [[\"service.reachable\", \"?service\", false]],
     \"repair\": \"Restart it with service.restart.\"},
    {\"id\": \"stale-endpoint\",
     \"predicts\": [[\"service.reachable\", \"?service\", true], [\"endpoint.current\", \"?service\", false]],
     \"repair\": \"Update its endpoint with service.update-endpoint.\"},
    {\"id\": \"credentials\",
     \"predicts\": [[\"service.reachable\", \"?service\", true], [\"endpoint.current\", \"?service\", true],
                  [\"auth.accepted\", \"?service\", false]],
     \"repair\": \"Rotate its credentials with service.rotate-credentials.\"}],
  \"probes\": [
    {\"tool\": \"service.ping\", \"arguments\": {\"service\": \"?service\"}},
    {\"tool\": \"service.config\", \"arguments\": {\"service\": \"?service\"}},
    {\"tool\": \"service.auth\", \"arguments\": {\"service\": \"?service\"}, \"cost\": 2}]}"))

(-> diagnosis-tests--context (configuration diagnosis-tests--world) tool-context)
(defun diagnosis-tests--context (configuration world)
  "Return a tool context running WORLD's tools under CONFIGURATION with approval."
  (make-instance 'tool-context
                 :configuration configuration :worker nil
                 :registry                       (diagnosis-tests--world-registry world)
                 :conversation                   (conversation-create configuration)
                 :command-authorization-function (lambda (command directory)
                                                   (declare (ignore command directory))
                                                   ':full-access)))

(-> diagnosis-tests--diagnose (configuration diagnosis-tests--world json-object) string)
(defun diagnosis-tests--diagnose (configuration world arguments)
  "Run diagnosis.run with ARGUMENTS in WORLD and return its report."
  (let ((result (tool-registry-execute-call
                 (diagnosis-tests--world-registry world)
                 (json-object "namespace" "diagnosis" "name" "run"
                              "arguments" (json-encode arguments))
                 (diagnosis-tests--context configuration world))))
    (unless (tool-result-success-p result)
      (error "diagnosis.run failed: ~A" (tool-result-content result)))
    (tool-result-content result)))

(-> diagnosis-tests--call (configuration diagnosis-tests--world string) json-object)
(defun diagnosis-tests--call (configuration world tool)
  "Call TOOL on the billing service in WORLD and return its decoded result."
  (let ((dot (position #\. tool)))
    (json-decode
     (tool-result-content
      (tool-registry-execute-call
       (diagnosis-tests--world-registry world)
       (json-object "namespace" (subseq tool 0 dot) "name" (subseq tool (1+ dot))
                    "arguments" (json-encode (json-object "service" "billing")))
       (diagnosis-tests--context configuration world))))))

(-> diagnosis-tests--call-with (string function) null)
(defun diagnosis-tests--call-with (label function)
  "Call FUNCTION with a configuration that runs diagnoses, asserting no provider request."
  (with-test-configuration (base root)
    (let ((configuration (configuration-copy base :working-directory root :diagnosis-p t))
          (requests      0))
      (diagnosis-tests--define-vocabulary configuration)
      (test-call-with-function-replacements
       (list (list 'provider-stream-turn
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf requests)
                     (error "Diagnoses must not reach a provider."))))
       (lambda ()
         (funcall function configuration)))
      (test-assert (zerop requests) (format nil "~A makes no provider request" label))))
  nil)

(-> test-diagnosis-display-names () null)
(defun test-diagnosis-display-names ()
  "Test diagnosis with display and canonical names in contracts and probes."
  (dolist (canonical-contract-p '(nil t))
    (dolist (canonical-probe-p '(nil t))
      (diagnosis-tests--call-with
       "mixed probe and contract names"
       (lambda (configuration)
         (let* ((world      (diagnosis-tests--world-create configuration "stale-endpoint" :wire-names-p t))
                (registry   (diagnosis-tests--world-registry world))
                (definition (diagnosis-tests--definition)))
           (loop for probe across (gethash "probes" definition)
                 for (display predicate field) in '(("service.ping" "service.reachable" "reachable")
                                                    ("service.config" "endpoint.current" "current")
                                                    ("service.auth" "auth.accepted" "accepted"))
                 for tool = (find display (tool-registry-tools registry)
                                  :key #'tool-display-name :test #'string=)
                 do (when canonical-contract-p
                      (knowledge-define-contract
                       configuration :id (substitute #\- #\. display)
                       :tool             (tool-canonical-name tool)
                       :when             (json-object "service" "?service")
                       :observes         (vector (json-object "predicate" predicate
                                                              "arguments" (vector "$arg.service"
                                                                                  (format nil "$result.~A" field))))))
                    (when canonical-probe-p
                      (setf (gethash "tool" probe) (tool-canonical-name tool))))
           (diagnosis-tests--diagnose configuration world definition)
           (let ((case (first (knowledge-base-diagnosis-cases
                               (knowledge-base-read configuration)
                               (knowledge--workspace configuration)))))
             (test-assert (equal (getf case :remaining) '("stale-endpoint"))
                          (format nil "contract canonical ~A, probe canonical ~A resolves the cause"
                                  canonical-contract-p canonical-probe-p))
             (test-assert (and (= 2 (length (getf case :probes)))
                               (every (lambda (probe) (getf probe :observed)) (getf case :probes)))
                          "both probes report their recorded observations")))))))
  nil)

(-> test-diagnosis-name-collisions () null)
(defun test-diagnosis-name-collisions ()
  "Test canonical names take precedence and ambiguous display aliases do not dispatch."
  (diagnosis-tests--call-with
   "canonical name collision"
   (lambda (configuration)
     (let* ((world    (diagnosis-tests--world-create configuration "stale-endpoint" :wire-names-p t))
            (registry (diagnosis-tests--world-registry world))
            (wire     (find "service.ping" (tool-registry-tools registry)
                            :key #'tool-display-name :test #'string=))
            (context  (diagnosis-tests--context configuration world)))
       (learning-tests--tool registry "service.ping"
                             (lambda (arguments)
                               (declare (ignore arguments))
                               (json-object "reachable" (json-false))))
       (knowledge--run-probe context (list :tool     (tool-canonical-name wire)
                                          :arguments '(("service" "billing"))))
       (test-assert (null (knowledge--established (knowledge-view-current configuration)
                                                 '("service.reachable" "billing" "?value")))
                    "a display contract for another canonical tool does not record facts")
       (knowledge--run-probe context '(:tool "service.ping" :arguments (("service" "billing"))))
       (test-assert (knowledge--established (knowledge-view-current configuration)
                                            '("service.reachable" "billing" :false))
                    "canonical dispatch wins over an earlier display alias"))))
  (diagnosis-tests--call-with
   "ambiguous display alias"
   (lambda (configuration)
     (let* ((world    (diagnosis-tests--world-create configuration nil :wire-names-p t))
            (registry (diagnosis-tests--world-registry world)))
       (tool-registry-register
        registry (make-instance 'diagnosis-tests--wire-named-tool
                                :namespace "other" :name "ping" :display "service.ping"
                                :description "A second tool with the same display alias."
                                :parameters  (tool-object-schema (json-object) nil)
                                :respond     (lambda (arguments)
                                               (declare (ignore arguments))
                                               (json-object "reachable" t))))
       (let ((probe (knowledge--run-probe
                     (diagnosis-tests--context configuration world)
                     '(:tool "service.ping" :arguments (("service" "billing"))))))
         (test-assert (eq ':failed (getf probe :status))
                      "an ambiguous display alias executes neither tool")))))
  nil)

(-> test-diagnosis-missing-probe-budget () null)
(defun test-diagnosis-missing-probe-budget ()
  "Test unavailable probes do not exhaust the run budget before a usable probe."
  (diagnosis-tests--call-with
   "missing probes"
   (lambda (configuration)
     (let* ((world      (diagnosis-tests--world-create configuration "unavailable"))
            (definition (diagnosis-tests--definition))
            (missing
              (loop for index below 3
                    for tool = (format nil "missing.ping~D" index)
                    do (knowledge-define-contract
                        configuration :id (format nil "missing-ping~D" index) :tool tool
                        :observes (vector (json-object "predicate" "service.reachable"
                                                       "arguments" #("$arg.service" "$result.reachable"))))
                    collect (json-object "tool" tool "cost" 0.01
                                         "arguments" (json-object "service" "?service")))))
       (setf (gethash "probes" definition)
             (concatenate 'vector missing (gethash "probes" definition)))
       (diagnosis-tests--diagnose configuration world definition)
       (let ((case (first (knowledge-base-diagnosis-cases
                           (knowledge-base-read configuration)
                           (knowledge--workspace configuration)))))
         (test-assert (equal (getf case :remaining) '("unavailable"))
                      "the available probe identifies the cause after three missing candidates")
         (test-assert (equal (mapcar (lambda (probe) (getf probe :tool)) (getf case :probes))
                             '("service.ping"))
                      "missing candidates consume no executions")))))
  nil)

(-> test-diagnosis-single-fact-hypotheses () null)
(defun test-diagnosis-single-fact-hypotheses ()
  "Test probes settle hypotheses that each predict one fact the others leave open."
  (loop for fault in '("unavailable" "stale-endpoint" "credentials")
        do (let ((fault fault))
             (diagnosis-tests--call-with
              (format nil "~A from single-fact hypotheses" fault)
              (lambda (configuration)
                (let* ((world  (diagnosis-tests--world-create configuration fault))
                       (report (diagnosis-tests--diagnose
                                configuration world
                                (json-decode "{\"parameters\": [\"service\"],
  \"bindings\": {\"service\": \"billing\"},
  \"hypotheses\": [
    {\"id\": \"unavailable\", \"predicts\": [[\"service.reachable\", \"?service\", false]]},
    {\"id\": \"stale-endpoint\", \"predicts\": [[\"endpoint.current\", \"?service\", false]]},
    {\"id\": \"credentials\", \"predicts\": [[\"auth.accepted\", \"?service\", false]]}],
  \"probes\": [
    {\"tool\": \"service.ping\", \"arguments\": {\"service\": \"?service\"}},
    {\"tool\": \"service.config\", \"arguments\": {\"service\": \"?service\"}},
    {\"tool\": \"service.auth\", \"arguments\": {\"service\": \"?service\"}}]}"))))
                  (test-assert (search (format nil "Diagnosis given: ~A~%" fault) report)
                               (format nil "probes leave only ~A: ~A" fault report)))))))
  nil)

(-> test-diagnosis-predictions () null)
(defun test-diagnosis-predictions ()
  "Test deciding predictions from facts and validating diagnoses."
  (diagnosis-tests--call-with
   "predictions"
   (lambda (configuration)
     (knowledge-tests--said configuration "service.reachable" '("billing" :true)
                            :quote "billing answers")
     (knowledge-tests--said configuration "auth.accepted" '("billing" :false)
                            :quote "billing never rejects us" :negated t)
     (knowledge-assert configuration '("endpoint.current" "billing" :true)
                       :basis ':model-proposal)
     (let ((view (knowledge-view-current configuration)))
       (loop for (prediction expected) in '((("service.reachable" "billing" :true) :confirmed)
                                            (("service.reachable" "billing" :false) :contradicted)
                                            ((:not ("service.reachable" "billing" :true)) :contradicted)
                                            (("service.reachable" "mail" :false) :unknown)
                                            (("endpoint.current" "billing" :false) :unknown)
                                            (("endpoint.current" "billing" :true) :unknown)
                                            (("auth.accepted" "billing" :false) :contradicted)
                                            ((:not ("auth.accepted" "billing" :false)) :confirmed))
             do (test-assert (eq (knowledge-prediction-status view prediction) expected)
                             (format nil "~S is ~(~A~)" prediction expected))))
     (let ((schema-function (lambda (predicate)
                              (knowledge-base-schema (knowledge-base-read configuration) predicate
                                                     :workspace (knowledge--workspace
                                                                 configuration)))))
       (loop for (description hypotheses) in
             `(("five hypotheses are too many"
                ,(loop for index below 5
                       collect (json-object "id" (format nil "h~D" index)
                                            "predicts" #(#("service.reachable" "billing" t)))))
               ("a hypothesis must predict something"
                (,(json-object "id" "vague" "predicts" #())))
               ("variables must be parameters"
                (,(json-object "id" "loose" "predicts" #(#("service.reachable" "?other" t))))))
             do (test-assert (handler-case
                                 (progn (knowledge-make-diagnosis :id "bad" :hypotheses hypotheses
                                                                  :schema-function schema-function)
                                        nil)
                               (knowledge-error () t))
                             description)))))
  nil)

(-> test-diagnosis-remote-service () null)
(defun test-diagnosis-remote-service ()
  "Test that discriminating probes find each remote-service fault, replacing speculative repairs."
  (let ((speculative 0)
        (diagnosed   0))
    (loop for (fault nil) in '(("unavailable" ("service.ping"))
                               ("stale-endpoint" ("service.ping" "service.config"))
                               ("credentials" ("service.ping" "service.config")))
          do (diagnosis-tests--call-with
              fault
              (lambda (configuration)
                (let ((world (diagnosis-tests--world-create configuration fault)))
                  ;; Without a diagnosis, repairs are tried in turn until the check passes.
                  (loop for (nil . repair) in *diagnosis-tests-repairs*
                        until (eq t (json-get (diagnosis-tests--call configuration world
                                                                     "service.check")
                                              "healthy"))
                        do (diagnosis-tests--call configuration world repair))
                  (incf speculative (length (diagnosis-tests--world-repairs world)))))))
    (loop for (fault expected-probes) in '(("unavailable" ("service.ping"))
                                           ("stale-endpoint" ("service.ping" "service.config"))
                                           ("credentials" ("service.ping" "service.config")))
          do (diagnosis-tests--call-with
              fault
              (lambda (configuration)
                (let* ((world  (diagnosis-tests--world-create configuration fault))
                       (report (diagnosis-tests--diagnose configuration world
                                                          (diagnosis-tests--definition)))
                       (case   (first (knowledge-base-diagnosis-cases
                                       (knowledge-base-read configuration)
                                       (knowledge--workspace configuration)))))
                  (test-assert (and (equal (getf case :remaining) (list fault))
                                    (equal (mapcar (lambda (probe) (getf probe :tool))
                                                   (getf case :probes))
                                           expected-probes)
                                    (search (format nil "Diagnosis remote-service: ~A. Repair: " fault)
                                            report))
                               (format nil "~A is found with ~{~A~^ then ~}" fault expected-probes))
                  (test-assert (= (1- 3) (length (getf case :eliminated)))
                               (format nil "~A's case records the facts ruling out the others" fault))
                  (diagnosis-tests--call configuration world
                                         (rest (assoc fault *diagnosis-tests-repairs*
                                                      :test #'string=)))
                  (test-assert (eq t (json-get (diagnosis-tests--call configuration world
                                                                      "service.check")
                                               "healthy"))
                               (format nil "the repair ~A's diagnosis names fixes it" fault))
                  (incf diagnosed (length (diagnosis-tests--world-repairs world)))))))
    (test-assert (and (= 3 diagnosed) (= 6 speculative))
                 (format nil "diagnosis needs ~D repairs where trying them in turn needs ~D"
                         diagnosed speculative)))
  (diagnosis-tests--call-with
   "ambiguous"
   (lambda (configuration)
     (let* ((world     (diagnosis-tests--world-create configuration "stale-endpoint" :config-p nil))
            (arguments (diagnosis-tests--definition)))
       ;; Without service.auth, only the missing service.config could distinguish the pair.
       (setf (gethash "probes" arguments) (subseq (gethash "probes" arguments) 0 2))
       (diagnosis-tests--diagnose configuration world arguments)
       (let ((case (first (knowledge-base-diagnosis-cases
                           (knowledge-base-read configuration)
                           (knowledge--workspace configuration)))))
         (test-assert (and (equal (getf case :remaining) '("stale-endpoint" "credentials"))
                           (equal (mapcar (lambda (probe) (getf probe :tool)) (getf case :probes))
                                  '("service.ping"))
                           (null (getf case :repair)))
                      "a case no available probe can settle stays open and names no repair")))))
  (diagnosis-tests--call-with
   "reuse"
   (lambda (configuration)
     (let ((world (diagnosis-tests--world-create configuration "stale-endpoint")))
       (diagnosis-tests--diagnose configuration world (diagnosis-tests--definition))
       (setf (diagnosis-tests--world-fault world) "credentials")
       (let* ((arguments (json-object "diagnosis" "remote-service"
                                      "bindings" (json-object "service" "billing")))
              (report    (diagnosis-tests--diagnose configuration world arguments))
              (case      (first (knowledge-base-diagnosis-cases (knowledge-base-read configuration)
                                                                (knowledge--workspace configuration)))))
         (test-assert (and (equal (mapcar (lambda (probe) (getf probe :tool)) (getf case :probes))
                                  nil)
                           (equal (getf case :remaining) '("stale-endpoint")))
                      "facts still holding are reused, so the saved diagnosis runs no probe")
         (test-assert (search "Diagnosis remote-service: stale-endpoint" report)
                      "a saved diagnosis runs by name with new bindings")))))
  (diagnosis-tests--call-with
   "literal-observation"
   (lambda (configuration)
     (let ((world (diagnosis-tests--world-create configuration nil)))
       (knowledge-define-schema configuration
                                (knowledge-make-schema
                                 :predicate "service.measurement"
                                 :arguments '((:name "service" :type :entity)
                                              (:name "value" :type :number))
                                 :key       '("service")))
       (knowledge-define-contract
        configuration :id "measure-service" :tool "service.measure"
        :observes (vector (json-object "predicate" "service.measurement"
                                       "arguments" #("billing" 42))))
       (learning-tests--tool (diagnosis-tests--world-registry world) "service.measure"
                             (lambda (arguments)
                               (declare (ignore arguments))
                               (json-object)))
       (let* ((report (diagnosis-tests--diagnose
                       configuration world
                       (json-object
                        "save_as" "measurement"
                        "hypotheses" (vector (json-object "id" "measured"
                                                          "predicts" #(#("service.measurement" "billing" 42)))
                                             (json-object "id" "other"
                                                         "predicts" #(#("service.measurement" "billing" 43))))
                        "probes" (vector (json-object "tool" "service.measure"
                                                       "arguments" (json-object))))))
              (case   (first (knowledge-base-diagnosis-cases (knowledge-base-read configuration)
                                                             (knowledge--workspace configuration)))))
         (test-assert (and (equal (getf case :remaining) '("measured"))
                           (eq (getf (first (getf case :probes)) :status) ':ok)
                           (search "service.measurement(billing, 42)" report))
                      "a numeric observation literal can select and settle a diagnosis")))))
  (diagnosis-tests--call-with
   "blocked-probe"
   (lambda (configuration)
     (let* ((world     (diagnosis-tests--world-create configuration "unavailable" :wire-names-p t))
            (arguments (diagnosis-tests--definition)))
       (knowledge-define-schema
        configuration (knowledge-make-schema :predicate  "probe.allowed"
                                              :arguments '((:name "service" :type :entity))))
       (knowledge-define-contract
        configuration :id "ping-precondition"
        :tool             (tool-canonical-name
                           (find "service.ping" (tool-registry-tools (diagnosis-tests--world-registry world))
                                 :key #'tool-display-name :test #'string=))
        :when             (json-object "service" "?service")
        :requires         #(#("probe.allowed" "?service")))
       (learning-tests--tool (diagnosis-tests--world-registry world) "service.ping-fallback"
                             (lambda (arguments)
                               (declare (ignore arguments))
                               (json-object "reachable" (json-false))))
       (knowledge-define-contract
        configuration :id "ping-fallback" :tool "service.ping-fallback"
        :observes (vector (json-object "predicate" "service.reachable"
                                       "arguments" #("billing" "$result.reachable"))))
       (setf (gethash "probes" arguments)
             (concatenate 'vector (gethash "probes" arguments)
                          (vector (json-object "tool" "service.ping-fallback"
                                               "arguments" (json-object)))))
       (diagnosis-tests--diagnose configuration world arguments)
       (let ((case (first (knowledge-base-diagnosis-cases (knowledge-base-read configuration)
                                                         (knowledge--workspace configuration)))))
         (test-assert (and (equal (getf case :remaining) '("unavailable"))
                           (equal (mapcar (lambda (probe) (getf probe :tool)) (getf case :probes))
                                  '("service.ping-fallback")))
                      "an inadmissible probe consumes no run when a usable discriminator exists")))))
  nil)

(-> test-diagnosis-setting () null)
(defun test-diagnosis-setting ()
  "Test that diagnosis.run is offered only with active knowledge and probes enabled."
  (with-test-configuration (base root)
    (loop for (mode diagnosis-p expected) in '(("active" t t) ("active" nil nil) ("passive" t nil))
          do (let ((registry (make-default-tool-registry
                              :configuration (configuration-copy base :working-directory root
                                                                      :knowledge-mode    mode
                                                                      :diagnosis-p       diagnosis-p))))
               (test-assert (eq expected
                                (and (find "diagnosis.run" (tool-registry-tools registry)
                                           :key #'tool-canonical-name :test #'string=)
                                     t))
                            (format nil "diagnosis.run is ~:[not ~;~]offered with ~A knowledge and probes ~:[off~;on~]"
                                    expected mode diagnosis-p))))
    (test-assert (find "diagnosis.run"
                       (tool-registry-tools (make-default-tool-registry
                                             :configuration (configuration-copy
                                                             base :working-directory root)))
                       :key #'tool-canonical-name :test #'string=)
                 "a fresh configuration offers diagnosis.run by default"))
  nil)

(-> test-diagnosis-commands () null)
(defun test-diagnosis-commands ()
  "Test listing saved diagnoses and recent cases with what decided them."
  (diagnosis-tests--call-with
   "commands"
   (lambda (configuration)
     (let ((application (make-instance 'application :configuration configuration
                                                    :conversation  (conversation-create configuration)))
           (world       (diagnosis-tests--world-create configuration "stale-endpoint")))
       (flet ((text (entry)
                (memory-source-tests--text entry)))
         (test-assert (search "No diagnoses yet." (text (application-diagnoses-entry application)))
                      "(diagnoses) says when there are none")
         (diagnosis-tests--diagnose configuration world (diagnosis-tests--definition))
         (let ((listing (text (application-diagnoses-entry application))))
           (test-assert (and (search "diagnoses  1 saved · 1 recent case" listing)
                             (search "remote-service  unavailable, stale-endpoint, credentials · probes service.ping, service.config, service.auth"
                                     listing)
                             (search "✓ remote-service service=billing → stale-endpoint" listing)
                             (search "repair: Update its endpoint with service.update-endpoint." listing)
                             (search "ruled out unavailable: " listing))
                        "(diagnoses) shows the cause found, its repair, and what ruled out the rest"))))))
  nil)
