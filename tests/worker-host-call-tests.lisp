(in-package #:autolith)

;;;; -- Worker Host Dispatch Tests --

(defclass worker-host-test-tool (tool)
  ((function
    :initarg :function
    :reader worker-host-test-tool-function
    :documentation "Fixture body receiving the actual dispatch context."))
  (:documentation "An observable ordinary tool for worker authority tests."))

(defmethod tool-execute ((tool worker-host-test-tool) (context tool-context)
                        (arguments hash-table))
  "Execute the fixture through the same generic boundary as production tools."
  (funcall (worker-host-test-tool-function tool) context arguments))

(-> worker-host-tests--register (tool-registry string function) tool)
(defun worker-host-tests--register (registry name function)
  "Register one ordinary fixture in REGISTRY."
  (tool-registry-register
   registry (make-instance 'worker-host-test-tool
                           :namespace "host-test" :name name
                           :description "Observable worker callback fixture."
                           :parameters (tool-object-schema (json-object) nil)
                           :function function)))

(-> worker-host-tests--fixture (function) null)
(defun worker-host-tests--fixture (function)
  "Call FUNCTION with a disposable configuration, registry, agent and worker context."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (make-default-tool-registry))
         (conversation (conversation-create configuration))
         (pool (lisp-worker-pool-create configuration))
         (agent (make-instance 'agent :configuration configuration
                              :conversation conversation :worker pool
                              :tool-registry registry))
         (context (make-instance 'tool-context :configuration configuration
                                :conversation conversation :worker pool
                                :registry registry :agent agent :call-id "origin-call")))
    (unwind-protect
         (funcall function context)
      (lisp-worker-pool-stop-all pool)
      (platform-delete-directory-tree *platform* root :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> worker-host-tests--call (tool-context string string) list)
(defun worker-host-tests--call (context name arguments)
  "Dispatch one correlated callback with explicit fixture capabilities."
  (worker-host--dispatch
   (list :tool name :arguments arguments)
   :context (list context (list name) (worker-host--bindings) "default" nil)
   :identity (list :session "fixture" :request-id 9 :call-id 3)
   :cancelled-p (constantly nil)))

(-> test-worker-host-dispatch-authority () null)
(defun test-worker-host-dispatch-authority ()
  "Exercise exact context, authorization, condition/restart and audit boundaries."
  (worker-host-tests--fixture
   (lambda (origin)
     (let* ((registry (tool-context-registry origin))
            (configuration (tool-context-configuration origin))
            (conversation (tool-context-conversation origin))
            (agent (tool-context-agent origin))
            (authorization-count 0)
            (effect-count 0)
            (command-function (lambda (command directory)
                                (test-assert (string= command "fixture-command") "command callback receives exact command")
                                (test-assert (eq directory (config :working-directory configuration)) "command directory identity")
                                (incf authorization-count)
                                ':deny))
            (tool-function (lambda (tool arguments)
                             (declare (ignore tool arguments))
                             (incf authorization-count)
                             ':deny))
            (context
              (make-instance 'tool-context :configuration configuration
                             :conversation conversation :registry registry
                             :worker (tool-context-worker origin) :agent agent
                             :call-id "origin-call"
                             :command-authorization-function command-function
                             :tool-authorization-function tool-function)))
       (worker-host-tests--register
        registry "authority"
        (lambda (actual arguments)
          (declare (ignore arguments))
          (test-assert (eq (tool-context-configuration actual) configuration) "original configuration identity")
          (test-assert (eq (tool-context-conversation actual) conversation) "original conversation identity")
          (test-assert (eq (tool-context-agent actual) agent) "original agent identity")
          (test-assert (eq (tool-context-command-authorization-function actual) command-function) "exact command callback")
          (test-assert (eq (tool-context-tool-authorization-function actual) tool-function) "exact tool callback")
          (test-assert (search "worker:origin-call:fixture:9:3" (tool-context-call-id actual)) "correlated tool identity")
          (test-assert (getf *worker-host-tool-policy* :restricted-p) "nested callback requests inherit capability restriction")
          (test-assert (equal (getf *worker-host-tool-policy* :allowlist) '("host-test.authority")) "nested callback allowlist")
          (if (and (eq (tool-context-authorize-command actual "fixture-command"
                                                       (config :working-directory configuration)) ':full-access)
                   (eq (tool-context-authorize-tool actual
                                                    (tool-registry-find registry "host-test" "authority")
                                                    (json-object)) ':allow))
              (progn (incf effect-count) (tool-success "unexpected"))
              (progn
                (tool-context-authorize-tool actual
                                             (tool-registry-find registry "host-test" "authority")
                                             (json-object))
                (tool-failure "Denied by original authority." :code ':access-denied)))))
       (let ((result (worker-host-tests--call context "host-test.authority" "{}")))
         (test-assert (eq (getf result :code) ':access-denied) "capability grant cannot grant command/tool authority")
         (test-assert (= authorization-count 2) "both ordinary callbacks were invoked")
         (test-assert (zerop effect-count) "denied callback has no external effect"))
       (worker-host-tests--register
        registry "restart"
        (lambda (actual arguments)
          (declare (ignore actual arguments))
          (handler-bind ((worker-host-call-error
                           (lambda (condition)
                             (declare (ignore condition))
                             (invoke-restart 'supply-result))))
            (restart-case
                (worker-host--reject ':fixture "Recoverable fixture failure.")
              (supply-result () :report "Return a verified fixture result."
                (tool-success "recovered"))))))
       (test-assert (string= (getf (worker-host-tests--call context "host-test.restart" "{}") :content)
                             "recovered") "ordinary condition restart remains in host dynamic extent")
       (worker-host-tests--register
        registry "failure"
        (lambda (actual arguments)
          (declare (ignore actual arguments))
          (worker-host--reject ':fixture "Typed fixture failure.")))
       (test-assert (eq (getf (worker-host-tests--call context "host-test.failure" "{}") :code) ':fixture)
                    "normal registry translates structured condition")
       (worker-host-tests--register
        registry "cancel"
        (lambda (actual arguments)
          (declare (ignore actual arguments))
          (error 'job-aborted :identifier "origin-call" :reason ':cancelled
                             :message "Fixture cancellation.")))
       (test-assert
        (handler-case
            (progn (worker-host-tests--call context "host-test.cancel" "{}") nil)
          (job-aborted (condition)
            (eq (job-aborted-reason condition) ':cancelled)))
        "ordinary cancellation remains control flow through the callback")
       (let* ((admitted (allocate-instance (find-class 'mission-context)))
              (replacement (allocate-instance (find-class 'mission-context)))
              (captured nil)
              (function nil))
         (unwind-protect
              (progn
                (mission-context-bind admitted agent)
                (setf function (worker-host--bound-function
                                (lambda () (setf captured (mission--agent-context agent)))
                                :context context))
                (mission-context-bind replacement agent)
                (join-thread (make-thread function :name "Worker mission admission fixture"))
                (test-assert (eq captured admitted)
                             "execution thread retains the exact admitted mission across replacement"))
           (with-lock-held (*mission-contexts-lock*)
             (remhash agent *mission-contexts*))))
        (let* ((worker (lisp-worker-manager-worker (tool-context-worker context) "default"))
               (admitted (allocate-instance (find-class 'mission-context)))
               (replacement (allocate-instance (find-class 'mission-context))))
          (unwind-protect
               (dolist (expected (list nil admitted))
                 (with-lock-held (*mission-contexts-lock*)
                   (remhash agent *mission-contexts*))
                 (when expected (mission-context-bind expected agent))
                 (test-call-with-function-replacements
                  (list (list 'sbcl-workers:sbcl-worker-host-request
                              (lambda (worker operation arguments &key dispatcher context worker-context
                                                                    cancel-p request-limit result-limit)
                                (declare (ignore worker operation arguments dispatcher worker-context
                                                 cancel-p request-limit result-limit))
                                (mission-context-bind replacement agent)
                                (let ((bindings (third context)))
                                  (progv (mapcar #'first bindings) (mapcar #'rest bindings)
                                    (test-assert *worker-host-admission* "request captures an explicit admission snapshot")
                                    (test-assert (eq expected (mission--agent-context agent))
                                                 "callback and nested admission retain exact mission or explicit absence")))
                                '(:response :status :ok))))
                  (lambda ()
                    (worker-host-request worker ':eval '(:forms ("42")) :context context :tools nil))))
            (with-lock-held (*mission-contexts-lock*)
              (remhash agent *mission-contexts*))))
       (let ((records nil))
         (conversation-map-records conversation (lambda (record) (push record records)))
         (test-assert (= (count :worker-tool-call records :key #'first) 4) "durable callback audit admission")
         (test-assert (= (count :worker-tool-result records :key #'first) 4) "durable callback audit outcomes")
         (test-assert (find-if (lambda (record)
                                (and (eq (first record) ':worker-tool-result)
                                     (eq (getf (getf (rest record) :result) :code) ':unknown-outcome)))
                              records)
                      "interrupted host work has a durable correlated unknown outcome")
         (test-assert (every (lambda (record)
                              (or (not (member (first record) '(:worker-tool-call :worker-tool-result)))
                                  (equal (getf (rest record) :parent-call-id) "origin-call"))) records)
                      "callback audit links exact originating call")))))
  nil)

(-> test-worker-host-capabilities () null)
(defun test-worker-host-capabilities ()
  "Reject malformed, unavailable and self capabilities, including restricted turns."
  (worker-host-tests--fixture
   (lambda (context)
     (worker-host-tests--register (tool-context-registry context) "echo"
                                 (lambda (actual arguments)
                                   (declare (ignore actual arguments)) (tool-success "echo")))
     (dolist (requested (list "host-test.echo" #(12) #("unknown.tool") #("self.status")))
       (test-assert
        (handler-case
            (progn (worker-host--allowlist context (json-object "host-tools" requested)) nil)
          (worker-host-call-error () t)) "invalid requested capability rejected"))
     (let ((*worker-host-tool-policy* (list :restricted-p t :allowlist '("lisp.eval"))))
       (test-assert
        (handler-case
            (progn (worker-host--allowlist context (json-object "host-tools" #("host-test.echo"))) nil)
          (worker-host-call-error (condition)
            (eq (worker-host-call-error-code condition) ':capability-denied)))
        "originating restricted turn cannot escape through worker"))
     (multiple-value-bind (names enabled-p) (worker-host--allowlist context (json-object))
       (test-assert (and (null names) (null enabled-p)) "ordinary eval remains explicitly unenabled"))
     (test-assert
      (eq (getf (worker-host-tests--call context "lisp.eval" "{\"form\":\"42\"}") :code)
          ':worker-reentrancy) "same-worker eval rejected before admission")
     (test-assert
      (eq (getf (worker-host-tests--call context "lisp.source" "{\"target\":\"self\",\"symbol\":\"CL:CONS\"}") :code)
          ':capability-denied) "worker has no active-image authority"))))

(-> test-worker-host-persistent-transport () null)
(defun test-worker-host-persistent-transport ()
  "Exercise opted-in eval, compile, scratchpad, sequential callbacks and other REPLs."
  (worker-host-tests--fixture
   (lambda (context)
     (let* ((registry (tool-context-registry context))
            (effects 0)
            (worker (lisp-worker-manager-worker (tool-context-worker context) "default")))
       (test-assert
        (equal (getf (rest (lisp-worker-request worker :eval
                                                '(:forms ("(package-name *package*)")))) :values)
               '("\"AUTOLITH\""))
        "plain eval uses the configured Autolith package")
       (worker-host-tests--register
        registry "echo" (lambda (actual arguments)
                           (declare (ignore arguments))
                           (test-assert (eq (tool-context-agent actual) (tool-context-agent context)) "supervised callback thread retains agent")
                           (incf effects)
                           (tool-success (format nil "echo-~D" effects))))
       (let ((result
               (tool-registry-execute-call
                registry (json-object "namespace" "lisp" "name" "eval"
                                      "arguments" (json-encode
                                                   (json-object "forms"
                                                                #("(defparameter *worker-host-fixture-state* 10)"
                                                                  "(list (getf (autolith:worker-tool-call \"host-test.echo\" \"{}\") :content) (getf (autolith:worker-tool-call \"host-test.echo\" \"{}\") :content) (getf (autolith:worker-tool-context) :parent-call-id))")
                                                                "host-tools" #("host-test.echo")))) context)))
         (test-assert (tool-result-success-p result)
                      (format nil "ordinary eval opts into callbacks: ~A" (tool-result-content result)))
         (test-assert (search "echo-1" (tool-result-content result)) "first callback result")
         (test-assert (search "echo-2" (tool-result-content result)) "sequential callback result")
         (test-assert (search "origin-call" (tool-result-content result)) "portable informational inherited context"))
       (let ((result (worker-response-tool-result
                      (lisp-worker-request worker :eval (list :forms '("*worker-host-fixture-state*"))))))
         (test-assert (and (tool-result-success-p result) (search "10" (tool-result-content result))) "callback eval retains persistent heap"))
       (let ((result
               (tool-registry-execute-call
                registry (json-object "namespace" "lisp" "name" "eval"
                                      "arguments" (json-encode
                                                   (json-object "forms" #("(getf (autolith:worker-tool-call \"host-test.echo\" \"{}\") :content)")
                                                                "compile" t "host-tools" #("host-test.echo")))) context)))
         (test-assert (tool-result-success-p result) "compiled eval supports callbacks"))
       (let* ((path (lisp-scratchpad-path context "callback.lisp"))
              (text "(setf *worker-host-fixture-state* (getf (autolith:worker-tool-call \"host-test.echo\" \"{}\") :content))"))
         (ensure-directories-exist path)
         (with-open-file (stream path :direction ':output :if-exists ':supersede
                                :if-does-not-exist ':create :external-format ':utf-8)
           (write-string text stream))
         (let ((result (tool-registry-execute-call
                        registry (json-object "namespace" "lisp" "name" "scratchpad-run"
                                              "arguments" (json-encode
                                                           (json-object "path" "callback.lisp"
                                                                        "host-tools" #("host-test.echo")))) context)))
           (test-assert (tool-result-success-p result) "scratchpad supports explicit host tools")))
       (let ((result
               (worker-response-tool-result
                (worker-host-eval-request
                 worker ':eval
                  (list :forms '("(list (getf (autolith:worker-tool-call \"lisp.eval\" \"{\\\"forms\\\":[\\\"42\\\"]}\") :code) (getf (autolith:worker-tool-call \"lisp.eval\" \"{\\\"forms\\\":[\\\"(+ 20 22)\\\"],\\\"repl\\\":\\\"other\\\"}\") :content))"))
                 :context context :tools '("lisp.eval") :enabled-p t))))
         (test-assert (tool-result-success-p result) "nested host eval completes without deadlock")
         (test-assert (search "WORKER-REENTRANCY" (tool-result-content result)) "same worker returns helpful classification")
         (test-assert (search "42" (tool-result-content result)) "other worker evaluates through ordinary boundary"))
       (let ((result
               (worker-response-tool-result
                (lisp-worker-request worker ':eval
                                     (list :forms '("(handler-case (autolith:worker-tool-call \"host-test.echo\" \"{}\") (sbcl-workers:sbcl-worker-host-error (condition) (sbcl-workers:sbcl-worker-host-error-code condition)))"))))))
         (test-assert (search "UNAVAILABLE" (tool-result-content result)) "facade cannot retain capability beyond one eval"))
       (test-assert
        (equal (getf (rest (lisp-worker-request worker :eval
                                                '(:forms ("(package-name *package*)")))) :values)
               '("\"AUTOLITH\""))
        "plain eval retains configured package after callback bootstrap")
       (test-assert (= effects 4) "all opted-in callbacks execute exactly once"))))
  nil)
