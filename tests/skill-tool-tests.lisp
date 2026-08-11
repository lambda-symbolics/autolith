(in-package #:autolith)

;;;; -- Skill Selection Tool Tests --

(defclass skill-workflow-test-tool (tool)
  ()
  (:documentation "A deterministic tool called from executable Skill tests."))

(defclass skill-workflow-test-self-tool (self-tool)
  ()
  (:documentation "An active-image tool used to test workflow restriction."))

(defmethod tool-execute
    ((tool skill-workflow-test-tool)
     (context tool-context)
     (arguments hash-table))
  "Return the required test value from ARGUMENTS."
  (declare (ignore tool context))
  (tool-success (tool-argument arguments "value" :required t)))

(defmethod tool-execute
    ((tool skill-workflow-test-self-tool)
     (context tool-context)
     (arguments hash-table))
  "Return a marker when the workflow can reach this self tool."
  (declare (ignore tool context arguments))
  (tool-success "self tool reached"))

(-> skill-tool-tests--write (pathname string string) pathname)
(defun skill-tool-tests--write (root relative-path content)
  "Write CONTENT beneath ROOT at RELATIVE-PATH and return its pathname."
  (let ((pathname (merge-pathnames relative-path root)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-string content stream))
    pathname))

(-> skill-tool-tests--call
    (tool-registry tool-context string)
    tool-result)
(defun skill-tool-tests--call (registry context name)
  "Call skill.load through REGISTRY with exact NAME."
  (tool-registry-execute-call
   registry
   (json-object
    "namespace" "skill"
    "name" "load"
    "arguments" (json-encode (json-object "name" name)))
   context))

(-> skill-tool-tests--run
    (tool-registry tool-context string)
    tool-result)
(defun skill-tool-tests--run (registry context name)
  "Call skill.run through REGISTRY with exact NAME."
  (tool-registry-execute-call
   registry
   (json-object
    "namespace" "skill"
    "name" "run"
    "arguments" (json-encode (json-object "name" name)))
   context))

(-> skill-tool-tests--contribution
    (list string)
    (option context-contribution))
(defun skill-tool-tests--contribution (contributions identifier)
  "Return the contribution named IDENTIFIER from CONTRIBUTIONS."
  (find identifier
        contributions
        :key #'context-contribution-identifier
        :test #'string=))

(-> test-skill-load-tool () null)
(defun test-skill-load-tool ()
  "Test exact, ephemeral, child-safe Skill selection through skill.load."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (skill-root (merge-pathnames ".autolith/skills/" project))
         (secret-body
           "FOLLOW-THE-ALPHA-INSTRUCTION-BODY-ONLY-IN-REQUEST-CONTEXT")
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-with-working-directory
              base-configuration
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-load-tool"))
         (registry
           (skill-augment-tool-registry
            (make-instance 'tool-registry)))
         (tool (tool-registry-find registry "skill" "load"))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry)))
    (unwind-protect
         (progn
           (skill-tool-tests--write
            skill-root
            "alpha/SKILL.sexp"
            (format nil
                    "(:autolith-skill :version 1 :name \"alpha\" :description \"Apply the alpha workflow.\" :instructions ~S)~%"
                    secret-body))
           (skill-tool-tests--write
            skill-root
            "oversized/SKILL.sexp"
            (format nil
                    "(:autolith-skill :version 1 :name \"oversized\" :description \"Exercise deferred instruction reading.\" :instructions ~S)~%"
                    (make-string 256 :initial-element #\x)))
           (test-assert tool
                        "skill registry augmentation installs skill.load")
           (test-assert (eq tool
                            (tool-registry-find
                             (skill-augment-tool-registry registry)
                             "skill"
                             "load"))
                        "skill registry augmentation is idempotent")
           (test-assert (tool-child-safe-p tool)
                        "skill.load is available across the child-agent boundary")
           (test-assert
            (and (eq (tool-conversation-persistence tool) ':next-response)
                 (tool-provider-round-trip-barrier-p tool))
            "skill.load declares request-local persistence and a provider barrier")
           (let ((schema (tool-provider-schema tool)))
             (test-assert
              (and (string= (json-get schema "name") "load")
                   (equal (coerce
                           (json-get (json-get schema "parameters")
                                     "required")
                           'list)
                          '("name"))
                   (eq (json-get (json-get schema "parameters")
                                 "additionalProperties")
                       false))
              "skill.load exposes one required exact-name argument"))
           (let ((outside-turn
                   (skill-tool-tests--call registry context "alpha")))
             (test-assert
              (and (not (tool-result-success-p outside-turn))
                   (search "only while an agent turn is active"
                           (tool-result-content outside-turn)))
              "skill.load rejects selection that cannot survive a logical turn"))
           (let ((discovery-called-p nil)
                 (result nil))
             (test-call-with-function-replacements
              (list
               (list
                'skill-catalog-for-configuration
                (lambda (ignored-configuration)
                  (declare (ignore ignored-configuration))
                  (setf discovery-called-p t)
                  (error "Invalid names must not reach discovery."))))
              (lambda ()
                (setf result
                      (skill-tool-tests--call
                       registry
                       context
                       (make-string
                        (1+ *skill-name-character-limit*)
                        :initial-element #\a)))))
             (test-assert
              (and (not discovery-called-p)
                   (not (tool-result-success-p result))
                   (search "at most"
                           (tool-result-content result)))
              "skill.load rejects oversized names before filesystem discovery"))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Use the relevant workflow.")
            (lambda ()
              (let* ((before
                       (skill-request-contributions
                        configuration
                        conversation))
                     (result
                       (skill-tool-tests--call
                        registry
                        context
                        "alpha")))
                (test-assert
                 (null
                  (skill-tool-tests--contribution
                   before
                   "skill-selected-alpha"))
                 "an implicit skill is absent before skill.load selects it")
                (test-assert
                 (tool-result-success-p result)
                 "skill.load selects an exact discovered skill")
                (test-assert
                (equal *skill-logical-turn-selection-names* '("alpha"))
                 "skill.load accumulates selection in logical-turn state")
                (test-assert
                 (and (< (length (tool-result-content result)) 256)
                      (not (search secret-body
                                   (tool-result-content result)))
                      (null (tool-result-details result))
                      (null (tool-result-image-attachments result)))
                 "the request-local tool result contains only bounded confirmation")
                (let* ((after
                         (skill-request-contributions
                          configuration
                          conversation))
                       (selected
                         (skill-tool-tests--contribution
                          after
                          "skill-selected-alpha")))
                  (test-assert
                   (and selected
                        (search
                         secret-body
                         (context-contribution-instruction selected)))
                   "subsequent requests in the turn receive the complete body ephemerally"))
                (let ((duplicate
                        (skill-tool-tests--call
                         registry
                         context
                         "alpha")))
                  (test-assert
                   (and (tool-result-success-p duplicate)
                        (search "already selected"
                                (tool-result-content duplicate))
                        (equal *skill-logical-turn-selection-names*
                               '("alpha")))
                   "repeated selection is idempotent"))
                (let ((wrong-case
                        (skill-tool-tests--call
                         registry
                         context
                         "Alpha")))
                  (test-assert
                   (and (not (tool-result-success-p wrong-case))
                        (search "lowercase ASCII letters"
                                (tool-result-content wrong-case))
                        (equal *skill-logical-turn-selection-names*
                               '("alpha")))
                   "skill.load rejects names that differ only by case")))))
           (let ((*skill-instruction-character-limit* 128))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Use the large workflow.")
              (lambda ()
                (let ((result
                        (skill-tool-tests--call
                         registry
                         context
                         "oversized")))
                  (test-assert
                   (tool-result-success-p result)
                   "skill.load selects from metadata without reading the body")
                  (let ((warning
                          (skill-tool-tests--contribution
                           (skill-request-contributions
                            configuration
                            conversation)
                           "skill-warning-oversized")))
                    (test-assert
                     (and warning
                          (eq (context-contribution-class warning)
                              ':mandatory))
                     "deferred body failure becomes request-local warning")))))))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-skill-run-tool () null)
(defun test-skill-run-tool ()
  "Test executable Skill branching, tool calls, and reader boundaries."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (skill-root (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-with-working-directory
              base-configuration project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-run-tool"))
         (registry (make-instance 'tool-registry)))
    (skill-augment-tool-registry registry)
    (tool-registry-register
     registry
     (make-instance
      'skill-workflow-test-tool
      :namespace "workflow-test"
      :name "echo"
      :description "Return one workflow test value."
      :parameters
      (tool-object-schema
       (json-object "value" (tool-string-property "The returned value."))
       '("value"))))
    (tool-registry-register
     registry
     (make-instance
      'skill-workflow-test-self-tool
      :namespace "self"
      :name "workflow-probe"
      :description "Report whether a workflow can reach a self tool."
      :parameters (tool-object-schema (json-object) nil)))
    (let ((context
            (make-instance 'tool-context
                           :configuration configuration
                           :worker nil
                           :conversation conversation
                           :registry registry)))
      (unwind-protect
           (progn
             (skill-tool-tests--write
              skill-root
              "repeatable/SKILL.sexp"
              "(:autolith-skill :version 1 :name \"repeatable\" :description \"Run a branching workflow.\" :instructions \"Inspect before running.\" :workflow \"WORKFLOW.lisp\")")
             (skill-tool-tests--write
              skill-root
              "repeatable/WORKFLOW.lisp"
              "(progn (format t \"workflow output~%\") (if (= (+ 1 1) 2) (workflow-test.echo :value \"branch selected\") \"wrong branch\"))")
             (skill-tool-tests--write
              skill-root
              "plain/SKILL.sexp"
              "(:autolith-skill :version 1 :name \"plain\" :description \"Instructions only.\" :instructions \"Do the work.\")")
             (skill-tool-tests--write
              skill-root
              "no-self/SKILL.sexp"
              "(:autolith-skill :version 1 :name \"no-self\" :description \"Run without self tools.\" :instructions \"Do not inspect or mutate Autolith.\" :workflow \"WORKFLOW.lisp\" :workflow-self-tools nil)")
             (skill-tool-tests--write
              skill-root
              "no-self/WORKFLOW.lisp"
              "(self.workflow-probe)")
             (skill-tool-tests--write
              skill-root
              "escaped/SKILL.sexp"
              "(:autolith-skill :version 1 :name \"escaped\" :description \"Reject an escaping workflow.\" :instructions \"Do not run.\" :workflow \"WORKFLOW.lisp\")")
             (let ((outside
                     (skill-tool-tests--write
                      root
                      "outside-workflow.lisp"
                      "(error \"must not execute\")")))
               (sb-posix:symlink
                (namestring outside)
                (namestring
                 (merge-pathnames
                  "escaped/WORKFLOW.lisp" skill-root))))
             (let ((tool (tool-registry-find registry "skill" "run")))
               (test-assert tool
                            "skill registry augmentation installs skill.run")
               (test-assert (not (tool-child-safe-p tool))
                            "workflow execution remains a primary-agent capability"))
             (let ((result
                     (skill-tool-tests--run
                      registry context "repeatable")))
               (test-assert
                (and (tool-result-success-p result)
                     (search "workflow output" (tool-result-content result))
                     (search "branch selected" (tool-result-content result)))
                "a workflow branches and invokes a registered tool binding"))
             (let ((result (skill-tool-tests--run registry context "plain")))
               (test-assert
                (and (not (tool-result-success-p result))
                     (search "does not declare a workflow"
                             (tool-result-content result)))
                "instruction-only Skills cannot be executed"))
             (let ((result
                     (skill-tool-tests--run registry context "no-self")))
               (test-assert
                (and (not (tool-result-success-p result))
                     (not (search "self tool reached"
                                  (tool-result-content result))))
                "a no-self workflow cannot dispatch inspection or mutation tools"))
             (let ((result
                     (skill-tool-tests--run registry context "escaped")))
               (test-assert
                (and (not (tool-result-success-p result))
                     (search "outside its canonical root"
                             (tool-result-content result)))
                "a workflow symbolic link cannot escape its Skill directory"))
             (skill-tool-tests--write
              skill-root
              "repeatable/WORKFLOW.lisp"
              "(skill.run :name \"repeatable\")")
             (let ((result
                     (skill-tool-tests--run
                      registry context "repeatable")))
               (test-assert
                (and (not (tool-result-success-p result))
                     (search "recursion repeated"
                             (tool-result-content result)))
                "workflow recursion fails at a bounded explicit guard"))
             (let ((*skill-workflow-reader-evaluated-p* nil))
               (declare (special *skill-workflow-reader-evaluated-p*))
               (skill-tool-tests--write
                skill-root
                "repeatable/WORKFLOW.lisp"
                "#.(setf *skill-workflow-reader-evaluated-p* t)")
               (let ((result
                       (skill-tool-tests--run
                        registry context "repeatable")))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (null *skill-workflow-reader-evaluated-p*))
                  "workflow reading disables read-time evaluation"))))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore))))
  nil)
