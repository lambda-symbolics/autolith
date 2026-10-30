(in-package #:autolith)

;;;; -- Parent Knowledge Context Integration --

(-> test-parent-knowledge-context () null)
(defun test-parent-knowledge-context ()
  "Inject parent recall through the normal contributor and expand through the normal tool."
  (with-test-configuration (configuration root)
    (let* ((parent                 (autolith-supervisor:make-memory-store-context
                                    :memory-path    (merge-pathnames "trusted-memory.sexp" root)
                                    :knowledge-path (merge-pathnames "trusted-knowledge.sexp" root)
                                    :workspace (config :working-directory configuration) :mode "active"))
           (service                (autolith-supervisor:make-knowledge-service :context parent :session-id "context-tools"))
           (conversation           (conversation-create configuration :identifier "context-tools"))
           (adapter                (make-instance 'agent-protocol-knowledge :driver nil))
           (scope                  (make-instance 'agent-record-resource-scope :agenda nil :memory nil :papercut nil
                                                  :knowledge adapter :session nil))
           (context                (make-instance 'tool-context :configuration configuration :conversation conversation))
           (*context-contributors* nil))
      (autolith-supervisor:memory-remember parent :title "Atlas build" :content "Run the checked Atlas build.")
      (autolith-supervisor:knowledge-define-method
       parent :id "build-atlas" :task #("build" "?project")
       :steps (vector (json-object "step" "Build {?project}")))
      (conversation-append-user-message conversation "Atlas build")
      (register-context-contributor "related-memories" 'memory-related-context :source ':built-in)
      (with-recursive-lock-held (*agent-record-resource-scopes-lock*)
        (setf (gethash configuration *agent-record-resource-scopes*) scope))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'agent-protocol-knowledge-request
                   (lambda (remote operation &key view scope operation-id expected-revision query limit fields)
                      (declare (ignore remote operation-id expected-revision query limit))
                     (autolith-supervisor:knowledge-service-request
                      service
                      (agent-wire:make-knowledge-request
                        :operation operation :scope scope :view (or view "")
                       :fields (agent-protocol-knowledge--fields fields)))))
             (list 'memory-rank (lambda (&rest arguments)
                                  (declare (ignore arguments))
                                  (error "Child opened local memory ranking.")))
             (list 'knowledge-view-current (lambda (&rest arguments)
                                             (declare (ignore arguments))
                                             (error "Child opened a local knowledge view."))))
            (lambda ()
              (let* ((request       (make-instance 'request-context :configuration configuration
                                                    :conversation conversation :tool-namespaces #()))
                     (contributions (memory-related-context request))
                     (recall        (find "related-memories" contributions
                                          :key #'context-contribution-identifier :test #'equal)))
                (test-assert recall "The normal memory contributor accepts parent recall.")
                (test-assert (search "Atlas build" (context-contribution-evidence recall))
                             "Parent recall reaches the contribution evidence.")
                (test-assert (= 1 (memory-context-contribution-count recall))
                             "Parent recall retains its presentation count."))
              (let ((result (tool-execute (make-instance 'plan-expand-tool :namespace "plan" :name "expand"
                                                                  :description "Expand a task." :parameters (json-object)) context
                                          (json-object "task" #("build" "atlas")))))
                (test-assert (typep result 'tool-result) "The normal plan tool returns a result.")
                (test-assert (equal "Build atlas"
                                    (plan-step-text (first (workspace-plan-steps (plan-load configuration)))))
                             "The remote expansion writes the public workspace plan."))
              (let* ((term    '(:quantity 5/2 "km"))
                     (encoded (autolith-supervisor::knowledge-service--expansion-data term)))
                (test-assert (equal term (agent-knowledge-context--pattern encoded))
                             "Pattern terms preserve their exact type and rational value."))))
        (agent-record-resources-uninstall configuration))))
  nil)
