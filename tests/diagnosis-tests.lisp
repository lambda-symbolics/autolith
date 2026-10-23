(in-package #:autolith)

;;;; -- Diagnosis Tests --

;;; The remote-service family: a call to a service fails because the service
;;; is down, its configured endpoint is stale, or its credentials are
;;; rejected. Probes ping it, read its endpoint, and try to authenticate;
;;; contracts turn their results into facts. Each scenario runs with no
;;; provider at all.

(defparameter *diagnosis-tests-repairs*
  '(("unavailable" . "service.restart")
    ("stale-endpoint" . "service.update-endpoint")
    ("credentials" . "service.rotate-credentials"))
  "The repair tool each remote-service cause calls for.")

(defclass diagnosis-tests--world ()
  ((fault
    :initarg :fault
    :accessor diagnosis-tests--world-fault
    :documentation "What is wrong: unavailable, stale-endpoint, credentials, or NIL.")
   (registry
    :initarg :registry
    :reader diagnosis-tests--world-registry
    :documentation "The registry holding the service tools.")
   (repairs
    :initform nil
    :accessor diagnosis-tests--world-repairs
    :documentation "The repairs tried, newest first."))
  (:documentation "A remote service with one fault and the tools that probe and repair it."))

(-> diagnosis-tests--world-create (configuration (option string) &key (:config-p boolean))
    diagnosis-tests--world)
(defun diagnosis-tests--world-create (configuration fault &key (config-p t))
  "Return a world under CONFIGURATION whose service has FAULT; without CONFIG-P its endpoint cannot be read."
  (let* ((registry (make-default-tool-registry :configuration configuration))
         (world (make-instance 'diagnosis-tests--world :fault fault :registry registry)))
    (flet ((fault-p (name)
             (equal (diagnosis-tests--world-fault world) name)))
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
                     (tool tool))
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
                                   :key '("service"))))
  (loop for (tool predicate field) in '(("service.ping" "service.reachable" "reachable")
                                        ("service.config" "endpoint.current" "current")
                                        ("service.auth" "auth.accepted" "accepted"))
        do (knowledge-define-contract
            configuration
            :id (substitute #\- #\. tool) :tool tool
            :when (json-object "service" "?service")
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
                 :registry (diagnosis-tests--world-registry world)
                 :conversation (conversation-create configuration)
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
          (requests 0))
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


(-> test-diagnosis-predictions () null)
(defun test-diagnosis-predictions ()
  "Test deciding predictions from facts and validating diagnoses."
  (diagnosis-tests--call-with
   "predictions"
   (lambda (configuration)
     (knowledge-tests--said configuration "service.reachable" '("billing" :true)
                            "billing answers")
     (knowledge-tests--said configuration "auth.accepted" '("billing" :false)
                            "billing never rejects us" :negated t)
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
        (diagnosed 0))
    (loop for (fault expected-probes) in '(("unavailable" ("service.ping"))
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
                (let* ((world (diagnosis-tests--world-create configuration fault))
                       (report (diagnosis-tests--diagnose configuration world
                                                          (diagnosis-tests--definition)))
                       (case (first (knowledge-base-diagnosis-cases
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
     (let* ((world (diagnosis-tests--world-create configuration "stale-endpoint" :config-p nil))
            (report (diagnosis-tests--diagnose configuration world (diagnosis-tests--definition))))
       (test-assert (and (search "Diagnosis remote-service is still open: stale-endpoint, credentials remain"
                                 report)
                         (search "service.config" report)
                         (search "failed" report)
                         (null (getf (first (knowledge-base-diagnosis-cases
                                             (knowledge-base-read configuration)
                                             (knowledge--workspace configuration)))
                                     :repair)))
                    "a case no available probe can settle stays open and names no repair"))))
  (diagnosis-tests--call-with
   "reuse"
   (lambda (configuration)
     (let ((world (diagnosis-tests--world-create configuration "stale-endpoint")))
       (diagnosis-tests--diagnose configuration world (diagnosis-tests--definition))
       (setf (diagnosis-tests--world-fault world) "credentials")
       (let* ((arguments (json-object "diagnosis" "remote-service"
                                      "bindings" (json-object "service" "billing")))
              (report (diagnosis-tests--diagnose configuration world arguments))
              (case (first (knowledge-base-diagnosis-cases (knowledge-base-read configuration)
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
                                 :key '("service")))
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
              (case (first (knowledge-base-diagnosis-cases (knowledge-base-read configuration)
                                                           (knowledge--workspace configuration)))))
         (test-assert (and (equal (getf case :remaining) '("measured"))
                           (eq (getf (first (getf case :probes)) :status) ':ok)
                           (search "service.measurement(billing, 42)" report))
                      "a numeric observation literal can select and settle a diagnosis")))))
  (diagnosis-tests--call-with
   "blocked-probe"
   (lambda (configuration)
     (let* ((world (diagnosis-tests--world-create configuration "unavailable"))
            (arguments (diagnosis-tests--definition)))
       (knowledge-define-schema
        configuration (knowledge-make-schema :predicate "probe.allowed"
                                              :arguments '((:name "service" :type :entity))))
       (knowledge-define-contract
        configuration :id "service-ping" :tool "service.ping"
        :when (json-object "service" "?service")
        :requires #(#("probe.allowed" "?service"))
        :observes (vector (json-object "predicate" "service.reachable"
                                       "arguments" #("$arg.service" "$result.reachable"))))
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
                                                                      :knowledge-mode mode
                                                                      :diagnosis-p diagnosis-p))))
               (test-assert (eq expected
                                (and (find "diagnosis.run" (tool-registry-tools registry)
                                           :key #'tool-canonical-name :test #'string=)
                                     t))
                            (format nil "diagnosis.run is ~:[not ~;~]offered with ~A knowledge and probes ~:[off~;on~]"
                                    expected mode diagnosis-p)))))
  nil)

(-> test-diagnosis-commands () null)
(defun test-diagnosis-commands ()
  "Test listing saved diagnoses and recent cases with what decided them."
  (diagnosis-tests--call-with
   "commands"
   (lambda (configuration)
     (let ((application (make-instance 'application :configuration configuration
                                                    :conversation (conversation-create configuration)))
           (world (diagnosis-tests--world-create configuration "stale-endpoint")))
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
