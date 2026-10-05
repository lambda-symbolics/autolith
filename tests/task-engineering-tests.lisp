(in-package #:autolith)

;;;; -- Engineering Result Contracts --

(-> task-engineering-tests--result () json-object)
(defun task-engineering-tests--result ()
  "Return a representative engineering result with honest failed check evidence."
  (json-object
   "schemaVersion" 1
   "baseline" (json-object "kind" "git" "workspace" "/project/" "revision" "abc123")
   "change" (json-object "kind" "commit-range" "reference" "abc123..def456")
   "paths" (vector "src/example.lisp")
   "symbols" (vector "example")
   "checks" (vector (json-object "command" "./script/check --suite example"
                                "status" "failed" "summary" "One failing assertion"
                                "evidence" (vector "workspace:check.log")))
   "evidence" (vector "workspace:check.log")
   "issues" (vector "Check failure needs parent review")
   "integrationNotes" "Integrate after resolving the reported failure."))

(-> test-task-engineering-native-contract () null)
(defun test-task-engineering-native-contract ()
  "Exercise native role expansion, extensions, provider projection and provenance."
  (with-test-configuration (configuration)
    (let* ((pathname (merge-pathnames "agents/engineering.sexp"
                                     (test-configuration-root configuration)))
           (definition
             (task-parse-agent-file
              (task-tests--write-text
               pathname
               "(:name \"engineering\" :description \"Engineering role\" :instructions \"Return engineering evidence.\" :output (:type :engineering :properties ((\"review\" (:type :string))) :required (\"review\")))")
              ':project))
           (schema (task-agent-definition-output definition))
           (result (task-engineering-tests--result))
           (projection (task-output-schema->json schema)))
      (test-assert (not (task-output-schema-valid-p result schema))
                   "role-specific required fields extend the common contract")
      (setf (gethash "review" result) "Reviewed independently")
      (test-assert (task-output-schema-valid-p result schema)
                   "native engineering roles accept the common and extended fields")
      (test-assert
       (and (gethash "baseline" (gethash "properties" projection))
            (gethash "review" (gethash "properties" projection)))
       "provider JSON Schema exposes common and role-specific properties")
      (let ((condition
              (handler-case
                  (progn
                    (task-parse-agent-file
                    (task-tests--write-text
                     pathname
                     "(:name \"engineering\" :description \"Invalid extension\" :instructions \"Override a common field.\" :output (:type :engineering :properties ((\"baseline\" (:type :string)))))")
                     ':project)
                    nil)
                (task-agent-definition-error (condition) condition))))
        (test-assert
         (and condition
              (eq (task-agent-definition-error-source condition) ':project)
              (eq (task-agent-definition-error-field condition) ':output)
              (equal (task-agent-definition-error-pathname condition) pathname)
              (string= (task-agent-definition-error-definition-name condition)
                       "engineering"))
         "common field overrides fail with native role provenance")))
    (let ((role (find "engineering" (task-bundled-agent-definitions)
                      :key #'task-agent-definition-name :test #'string=)))
      (test-assert
       (and role (task-output-schema-valid-p
                  (task-engineering-tests--result)
                  (task-agent-definition-output role)))
       "the optional bundled engineering role uses the shared contract")))
  nil)

(-> test-task-engineering-yield-transport () null)
(defun test-task-engineering-yield-transport ()
  "Exercise invalid yields, explicit failure, and durable engineering transport."
  (with-test-configuration (configuration)
    (let ((definition
            (task-agent-definition-create
             :name "engineering-test" :description "Engineering evidence"
             :instructions "Report the observed results." :output ':engineering)))
      (dolist (field '("schemaVersion" "baseline" "change" "paths" "checks"
                       "evidence" "issues" "integrationNotes"))
        (let* ((fixture (task-tests--yield-fixture
                         configuration definition (format nil "missing-~A" field)))
               (data (task-engineering-tests--result)))
          (remhash field data)
          (test-assert
           (and (not (tool-result-success-p
                      (task-tests--execute-yield
                       fixture (json-encode (json-object "status" "success" "data" data)))))
                (not (task-completion-called-p (getf fixture :completion))))
           (format nil "missing common field ~A cannot complete a successful task" field))))
      (dolist (case '(("schemaVersion" 2) ("paths" "not-an-array")
                      ("change" "not-an-object")))
        (let* ((fixture (task-tests--yield-fixture
                         configuration definition (format nil "invalid-~A" (first case))))
               (data (task-engineering-tests--result)))
          (setf (gethash (first case) data) (second case))
          (test-assert
           (not (tool-result-success-p
                 (task-tests--execute-yield
                  fixture (json-encode (json-object "status" "success" "data" data)))))
           (format nil "invalid common field ~A is rejected" (first case)))))
      (dolist (status '("success"))
        (let* ((fixture (task-tests--yield-fixture
                         configuration definition (format nil "engineering-~A" status)))
               (data (task-engineering-tests--result))
               (arguments (json-object "status" status "data" data)))
          (test-assert
           (tool-result-success-p
            (task-tests--execute-yield fixture (json-encode arguments)))
           "a contract-valid result may report observed check failure honestly")
          (let* ((durable
                   (task--assemble-child-result
                    (getf fixture :job) (agent-test-result "engineering-result" nil)
                    (getf fixture :child) (getf fixture :conversation)
                    (getf fixture :completion)))
                 (pathname (task--write-result-artifact (getf fixture :job) durable))
                 (restored
                   (with-open-file (stream pathname :external-format ':utf-8)
                     (let ((*read-eval* nil)) (read stream)))))
            (test-assert
             (and (getf restored :structured-output-present-p)
                  (equal (getf restored :structured-output) (task-json->sexp data))
                  (task-output-schema-valid-p
                   (task-sexp->json (getf restored :structured-output))
                   (task-agent-definition-output definition)))
             "engineering data survives the existing durable task artifact boundary"))))
      (let ((fixture (task-tests--yield-fixture
                      configuration definition "engineering-no-artifact")))
        (test-assert
         (tool-result-success-p
          (task-tests--execute-yield
           fixture "{\"status\":\"failed\",\"error\":\"Cannot inspect the workspace\"}"))
         "a failed child can terminate before an engineering artifact is available"))))
  nil)
