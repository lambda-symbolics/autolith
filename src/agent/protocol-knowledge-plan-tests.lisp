(in-package #:autolith)

;;;; -- Normal Plan Tools Against Parent Knowledge --

(-> agent-knowledge-plan-test--call (tool-registry tool-context string &key (:arguments hash-table)) tool-result)
(defun agent-knowledge-plan-test--call (registry context name &key (arguments (json-object)))
  "Invoke a registered normal plan tool, including its runtime contract boundary."
  (tool-registry-execute-call registry
                              (json-object "namespace" "plan" "name" name
                                           "arguments" (json-encode arguments)) context))

(-> agent-knowledge-plan-test--client
    (t pathname &key (:root pathname) (:parent memory-store-context) (:reopen function)) integer)
(defun agent-knowledge-plan-test--client (driver private-file &key root parent reopen)
  "Exercise normal tools and context with an unusable child graph root."
  (let* ((configuration (make-configuration :provider-validation-p nil :knowledge-mode "active"
                                           :working-directory root :data-root private-file :config-root private-file
                                           :state-root (merge-pathnames "agent-state/" root)))
         (conversation  (conversation-create configuration :identifier    "plan-client"
                                                            :storage-root (merge-pathnames "conversation/" root)))
         (context       (make-instance 'tool-context :configuration configuration :conversation conversation :worker nil))
         (registry      (make-instance 'tool-registry))
         (raw           (vector (json-object "step" "Use {?value}" "status" "doing" "id" "build"
                                             "requires" #(#("build.value" "?value"))
                                             "verify" #("build.verified" "atlas"))))
         (checks        0))
    (default-tools--register-plan registry)
    (agent-record-resources-install driver configuration)
    (unwind-protect
         (labels ((check (value &optional detail)
                    (incf checks)
                    (assert value () "Normal parent plan check ~D failed: ~A" checks detail))

                  (call-plan (name &optional (arguments (json-object)))
                    (agent-knowledge-plan-test--call registry context name :arguments arguments))

                  (advice ()
                    (plan-changes-context (make-instance 'request-context :configuration configuration
                                                        :conversation conversation :tool-namespaces #()))))
           (conversation-append-user-message conversation "Inspect build")
           (let ((plain (call-plan "update" (json-object "steps" (vector (json-object "step" "Plain" "status" "pending"))))))
             (check (tool-result-success-p plain) (tool-result-content plain)))
           (let ((blocked (call-plan "update" (json-object "steps" raw))))
             (check (tool-result-success-p blocked))
             (check (search "blocked" (tool-result-content blocked))))
           (check (null (advice)))
           (multiple-value-bind (claim support)
               (autolith-supervisor:knowledge-assert parent '("build.value" 1/3) :basis ':assumption)
             (declare (ignore claim))
             (conversation-append-user-message conversation "Use current facts")
             (let ((change (advice)))
               (check (typep change 'plan-change-contribution))
               (check (search (knowledge-term-display 1/3) (context-contribution-evidence change)))
               (check (eq change (advice))))
             (let ((ready (call-plan "list")))
               (check (tool-result-success-p ready))
               (check (search (knowledge-term-display 1/3) (tool-result-content ready))))
             (setf (gethash "status" (aref raw 0)) "done")
             (let* ((done  (call-plan "update" (json-object "steps" raw)))
                    (plan  (plan-load configuration))
                    (token (getf (plan-step-receipt (first (workspace-plan-steps plan))) :parent-token)))
               (check (tool-result-success-p done))
               (check (non-empty-string-p token))
               (check (search "not observed" (tool-result-content done)))
               (autolith-supervisor:knowledge-assert parent '("build.verified" "atlas") :basis ':assumption)
               (conversation-append-user-message conversation "Check completion")
               (check (typep (advice) 'plan-change-contribution))
               (check (not (search "not observed" (tool-result-content (call-plan "list")))))
               (funcall reopen)
               (agent-record-resources-uninstall configuration)
               (setf configuration (configuration-copy configuration)
                     context (make-instance 'tool-context :configuration configuration :conversation conversation :worker nil))
               (agent-record-resources-install driver configuration)
               (check (equal token (getf (plan-step-receipt
                                          (first (workspace-plan-steps (plan-load configuration)))) :parent-token)))
               (check (tool-result-success-p (call-plan "list")))
               (autolith-supervisor:knowledge-retract parent support :reason "Build changed")
               (autolith-supervisor:knowledge-assert parent '("build.value" 2/3) :basis ':assumption)
               (conversation-append-user-message conversation "Check old build assumptions")
               (let ((change (advice)) (listed (call-plan "list")))
                 (check (typep change 'plan-change-contribution))
                 (check (search "no longer holds" (context-contribution-evidence change)))
                 (check (tool-result-success-p listed))
                 (check (search (format nil "Use ~A" (knowledge-term-display 1/3)) (tool-result-content listed)))
                 (check (search "no longer holds" (tool-result-content listed))))))
           (dolist (mode '("passive" "off"))
             (agent-record-resources-uninstall configuration)
             (setf configuration (configuration-copy configuration :knowledge-mode mode)
                   context (make-instance 'tool-context :configuration configuration :conversation conversation :worker nil))
             (agent-record-resources-install driver configuration)
             (conversation-append-user-message conversation mode)
             (check (null (advice)))
             (let ((listed (call-plan "list")))
               (check (tool-result-success-p listed))
               (check (search "no longer holds" (tool-result-content listed)))))

           (let ((expanded (call-plan "expand" (json-object "task" #("build" "atlas")))))
             (check (tool-result-success-p expanded) (tool-result-content expanded))
             (check (search (knowledge-term-display 2/3) (tool-result-content expanded))))
           (let ((bad (call-plan "update"
                                 (json-object "steps" (vector (json-object "step" "Unknown" "status" "pending"
                                                                          "requires" #(#("unknown" "x"))))))))
             (check (not (tool-result-success-p bad))))
           (let ((cleared (call-plan "update" (json-object "steps" #()))))
             (check (tool-result-success-p cleared))
             (check (null (plan-load configuration))))

           (setf (slot-value (agent-knowledge-context-scope configuration) 'knowledge) nil)
           (let ((plain (call-plan "update" (json-object "steps" (vector (json-object "step" "Plain optional" "status" "pending"))))))
             (check (tool-result-success-p plain) (tool-result-content plain))
             (check (tool-result-success-p (call-plan "list"))))
           (check (not (tool-result-success-p (call-plan "update" (json-object "steps" raw)))))
           (check (null (advice)))
           (agent-record-resources-uninstall configuration)
           (setf configuration (configuration-copy configuration :knowledge-mode "active"))
           (agent-record-resources-install driver configuration)
           (setf (slot-value (agent-knowledge-context-scope configuration) 'knowledge) nil)
           (remhash (conversation-identifier conversation) *plan-context-turn-changes*)
           (check (null (advice)) "A plain plan needs no optional knowledge adapter.")
           (plan-clear configuration)
           (check (null (advice)) "An empty plan needs no optional knowledge adapter.")
           checks)
      (agent-record-resources-uninstall configuration)
      (remhash (conversation-identifier conversation) *plan-context-turn-changes*))))

(-> run-agent-protocol-plan-tests () integer)
(defun run-agent-protocol-plan-tests ()
  "Run normal plan paths over framed bytes against the real parent service."
  (let* ((root         (merge-pathnames (format nil "plan-client-~A/" (make-identifier)) (uiop:temporary-directory)))
         (private-file (merge-pathnames "private/" root))
         (parent       (autolith-supervisor:make-memory-store-context
                        :memory-path (merge-pathnames "memory.sexp" root)
                        :knowledge-path (merge-pathnames "knowledge.sexp" root) :workspace root :mode "active"))
         (service      (autolith-supervisor:make-knowledge-service :context parent :session-id "plan-client"))
         (checks       0))
    (ensure-directories-exist root)
    (with-open-file (stream (merge-pathnames "private" root) :direction ':output :if-exists ':error)
      (write-line "The child private root is not a directory." stream))
    (autolith-supervisor:knowledge-define-schema parent
     (autolith-supervisor:knowledge-make-schema :predicate "build.value" :arguments '((:name "value" :type :number))))
    (autolith-supervisor:knowledge-define-schema parent
     (autolith-supervisor:knowledge-make-schema :predicate "build.verified" :arguments '((:name "project" :type :entity))))

    (autolith-supervisor:knowledge-define-method
     parent :id "build-method" :task #("build" "?project")
     :when  #(#("build.value" "?value"))
     :steps (vector (json-object "step" "Method {?value}"
                                "requires" #(#("build.value" "?value")))))
    (unwind-protect
         (agent-protocol-test--peers
          (lambda (connection)
            (let ((welcome (agent-protocol-test--welcome)))
              (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1" "knowledge.v1"))
              (autolith-supervisor::parent-connection-handshake connection welcome))
            (loop for envelope = (autolith-supervisor::parent-connection-receive connection)
                  do (case (agent-wire:envelope.body-case envelope)
                       (agent-wire:output
                        (return))
                       (agent-wire:knowledge-request
                        (let ((reply
                                (handler-case
                                    (agent-wire:make-envelope
                                     :knowledge-response
                                     (autolith-supervisor:knowledge-service-request
                                      service (agent-wire:envelope.knowledge-request envelope)))
                                  (autolith-supervisor:knowledge-service-error (condition)
                                    (agent-wire:make-envelope :failure
                                     (agent-wire:make-failure
                                      :code (autolith-supervisor:knowledge-service-error-code condition)))))))
                          (setf (agent-wire:envelope.reply-to reply) (agent-wire:envelope.message-id envelope))
                          (autolith-supervisor::parent-connection-send connection reply)))
                       (otherwise
                        (assert nil)))))
          (lambda (client)
            (agent-protocol-handshake client :features '("session.v1" "records.v1" "knowledge.v1"))
            (let ((driver (agent-protocol-driver-start client)))
              (unwind-protect
                   (setf checks
                         (agent-knowledge-plan-test--client
                          driver private-file :root root :parent parent
                          :reopen (lambda ()
                                    (setf service
                                          (autolith-supervisor:make-knowledge-service
                                           :context    (autolith-supervisor:make-memory-store-context
                                                        :memory-path    (merge-pathnames "memory.sexp" root)
                                                        :knowledge-path (merge-pathnames "knowledge.sexp" root)
                                                        :workspace root :mode "active")
                                           :session-id "plan-client")))))
                (agent-protocol-output client ':diagnostic "done")
                (agent-protocol-driver-close driver)
                (agent-protocol-driver-join driver)))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
    (format t "Normal parent plan: ~D checks passed.~%" checks)
    checks))
