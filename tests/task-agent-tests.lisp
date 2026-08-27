(in-package #:autolith)

;;;; -- Native Role Contract Tests --

(-> test-task-agent-native-reader () null)
(defun test-task-agent-native-reader ()
  "Test exact, safe, diagnostic-rich parsing of native role files."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (directory     (merge-pathnames "agents/" root)))
    (unwind-protect
         (progn
           (let* ((pathname
                    (merge-pathnames "native.sexp" directory))
                  (definition
                    (task-parse-agent-file
                     (task-tests--write-native-form
                      pathname
                      (task-tests--role-form
                       "native" "Native role" "Use native Lisp data."
                       :tools '("resource.read")
                       :blocking-p t))
                     :project)))
             (test-assert
              (and (string= (task-agent-definition-name definition) "native")
                   (string= (task-agent-definition-instructions definition)
                            "Use native Lisp data.")
                   (equal (task-agent-definition-tools definition)
                          '("resource.read"))
                   (task-agent-definition-blocking-p definition)
                   (eq (task-agent-definition-source definition) :project)
                   (equal (task-agent-definition-pathname definition)
                          pathname))
              "one .sexp form creates a complete native role definition"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "deterministic.sexp" directory)
                     "(:name \"deterministic\" :description \"Deterministic reader\" :instructions \"Ignore ambient reader state.\" :output (:type :number :enum (10 1.5)))"))
                  (definition
                    (let ((*read-base* 16)
                          (*read-suppress* t)
                          (*read-default-float-format* 'single-float)
                          (*package* (find-package '#:common-lisp-user)))
                      (task-parse-agent-file pathname :project)))
                  (enum
                    (getf (task-agent-definition-output definition) :enum)))
             (test-assert
              (and (= (first enum) 10)
                   (typep (second enum) 'double-float))
              "native role parsing ignores ambient package, base, suppression, and float bindings"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "block-comment.sexp" directory)
                     "#| leading |#(:name\"block-comment\"#| outer #| nested |# comment |#:description\"Commented role\":instructions #| value |#\"Accept standard block comments.\")"))
                  (definition
                    (task-parse-agent-file pathname :project)))
             (test-assert
              (and (string= (task-agent-definition-name definition)
                            "block-comment")
                   (string= (task-agent-definition-description definition)
                            "Commented role")
                   (string= (task-agent-definition-instructions definition)
                            "Accept standard block comments."))
              "native role parsing accepts standard keyword, string, and block-comment adjacency"))
           (let* ((pathname
                    (merge-pathnames "deeply-nested.sexp" directory))
                  (source
                    (concatenate
                     'string
                     (make-string 129 :initial-element #\()
                     "nil"
                     (make-string 129 :initial-element #\))))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text pathname source)
                     :project)))
             (test-assert
              (search "depth"
                      (string-downcase
                       (princ-to-string
                        (task-agent-definition-error-cause condition))))
              "native role parsing rejects source nesting beyond its hard bound"))
           (setf *task-test-reader-evaluated-p* nil)
           (let* ((pathname
                    (merge-pathnames "reader-eval.sexp" directory))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text
                      pathname
                      "(:name \"reader-eval\" :description \"Unsafe\" :instructions #.(progn (setf *task-test-reader-evaluated-p* t) \"executed\"))")
                     :project)))
             (test-assert (not *task-test-reader-evaluated-p*)
                          "the native role reader binds *READ-EVAL* to NIL")
             (test-assert
              (typep (task-agent-definition-error-line condition)
                     '(integer 1))
              "reader failures retain a one-based source line"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "fresh-readtable.sexp" directory)
                     "!"))
                  (condition
                    (let ((*readtable* (copy-readtable nil)))
                      (set-macro-character
                       #\!
                       (lambda (stream character)
                         (declare (ignore stream character))
                         (task-tests--role-form
                          "fresh-readtable" "Inherited macro"
                          "This must not be accepted."))
                       nil
                       *readtable*)
                      (task-tests--agent-definition-error
                       pathname :project))))
             (test-assert
              (search "Non-keyword symbol"
                      (princ-to-string
                      (task-agent-definition-error-cause condition)))
               "the native role reader starts from a fresh standard readtable"))
           (let ((bare-name "AUTOLITH-TASK-READER-BARE-LEAK-71D21A")
                 (qualified-name
                   "AUTOLITH-TASK-READER-QUALIFIED-LEAK-71D21A")
                 (keyword-name
                   "AUTOLITH-TASK-READER-KEYWORD-LEAK-71D21A"))
             (test-assert
              (and (null (find-symbol bare-name '#:autolith))
                   (null (find-symbol qualified-name '#:autolith))
                   (null (find-symbol keyword-name '#:keyword)))
              "reader-pollution sentinels begin absent from global packages")
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "bare-symbol.sexp" directory)
               "(:name \"bare-symbol\" :description \"Bare symbol\" :instructions \"Reject and forget it.\" :tools (autolith-task-reader-bare-leak-71d21a))")
              :project)
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "qualified-symbol.sexp" directory)
               "(:name \"qualified-symbol\" :description \"Qualified symbol\" :instructions \"Reject before interning it.\" :tools (autolith::autolith-task-reader-qualified-leak-71d21a))")
              :project)
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "unknown-keyword.sexp" directory)
               "(:name\"unknown-keyword\":description\"Unknown keyword\":instructions\"Reject adjacent unknown keywords before interning them.\":autolith-task-reader-keyword-leak-71d21a\"rejected\")")
              :project)
             (test-assert
              (and (null (find-symbol bare-name '#:autolith))
                   (null (find-symbol qualified-name '#:autolith))
                   (null (find-symbol keyword-name '#:keyword)))
              "malformed role symbols never pollute project or keyword packages"))
           (let* ((pathname
                    (merge-pathnames "utf8-bound.sexp" directory))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text
                      pathname
                      (make-string 65537 :initial-element #\é))
                     :project)))
             (test-assert
              (search "byte bound"
                      (princ-to-string
                       (task-agent-definition-error-cause condition)))
              "the native role file limit counts consumed UTF-8 bytes"))
           (dolist
               (case
                '(("extra"
                   "(:name \"extra\" :description \"Extra\" :instructions \"First\")\n(:second t)"
                   nil t)
                  ("incomplete"
                   "(:name \"incomplete\" :description \"Incomplete\" :instructions"
                   nil t)
                  ("shared"
                   "(:name \"shared\" :description #1=\"Shared\" :instructions #1#)"
                   nil nil)
                  ("circular"
                   "(:name \"circular\" :description \"Circular\" :instructions \"Reject cycles.\" :tools #1=(\"resource.read\" . #1#))"
                   nil nil)
                  ("dotted"
                   "(:name \"dotted\" :description \"Dotted\" :instructions \"Reject tails.\" . :tail)"
                   nil t)
                  ("unknown"
                   "(:name \"unknown\" :description \"Unknown\" :instructions \"Reject fields.\" :type :string)"
                   :type t)
                  ("duplicate"
                   "(:name \"duplicate\" :description \"First\" :instructions \"Reject duplicates.\" :description \"Second\")"
                   :description t)))
             (destructuring-bind
                 (name contents expected-field expected-line-p)
                 case
               (let* ((pathname
                        (merge-pathnames
                         (format nil "~A.sexp" name)
                         directory))
                      (condition
                        (task-tests--agent-definition-error
                         (task-tests--write-text pathname contents)
                         :project)))
                 (test-assert
                  (and (typep condition 'task-agent-definition-error)
                       (equal (task-agent-definition-error-pathname condition)
                              pathname)
                       (eq (task-agent-definition-error-source condition)
                           :project)
                       (string=
                        (task-agent-definition-error-definition-name condition)
                        name)
                       (task-agent-definition-error-cause condition)
                       (if expected-field
                           (eq (task-agent-definition-error-field condition)
                               expected-field)
                           t)
                       (if expected-line-p
                           (typep (task-agent-definition-error-line condition)
                                  '(integer 1))
                           t))
                   (format nil
                           "~A native role input returns complete typed diagnostic metadata"
                           name))))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)


(-> test-task-agent-discovery-precedence () null)
(defun test-task-agent-discovery-precedence ()
  "Test project, user, site, and bundled role precedence remains fail-closed."
  (let* ((site-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-agent-site-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (base-configuration
           (progn
             (ensure-directories-exist site-root)
             (setf site-root
                   (uiop:ensure-directory-pathname (truename site-root)))
             (test-configuration :site-config-root site-root)))
         (root          (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root))
         (project-directory (merge-pathnames ".autolith/agents/" root))
         (user-directory
           (merge-pathnames "agents/"
                            (configuration-config-root configuration)))
         (site-directory (merge-pathnames "agents/" site-root)))
    (unwind-protect
         (progn
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" project-directory)
            (task-tests--role-form
             "scout" "Project scout" "Project instructions."))
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" user-directory)
            (task-tests--role-form
             "scout" "User scout" "User instructions."))
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" site-directory)
            (task-tests--role-form
             "scout" "Site scout" "Site instructions."))
           (task-tests--write-native-form
            (merge-pathnames "reviewer.sexp" user-directory)
            (task-tests--role-form
             "reviewer" "User reviewer" "Review as configured by the user."))
           (task-tests--write-native-form
            (merge-pathnames "reviewer.sexp" site-directory)
            (task-tests--role-form
             "reviewer" "Site reviewer" "Review as configured by the site."))
           (task-tests--write-native-form
            (merge-pathnames "librarian.sexp" site-directory)
            (task-tests--role-form
             "librarian" "Site librarian" "Research using site policy."))
           (task-tests--write-text
            (merge-pathnames "sonic.sexp" project-directory)
            "(:name \"sonic\" :description \"Missing instructions\")")
           (task-tests--write-native-form
            (merge-pathnames "sonic.sexp" user-directory)
            (task-tests--role-form
             "sonic" "User sonic" "This lower role must stay blocked."))
           (dolist (filename '("dupe.sexp" "DUPE.sexp"))
             (task-tests--write-native-form
              (merge-pathnames filename project-directory)
              (task-tests--role-form
               "dupe" "Duplicate role" "Reject normalized duplicates.")))
           (task-tests--write-native-form
            (merge-pathnames "dupe.sexp" user-directory)
            (task-tests--role-form
             "dupe" "Lower duplicate" "This lower role must stay blocked."))
           (multiple-value-bind (definitions diagnostics)
               (task-discover-agents configuration)
             (let ((scout
                     (task-find-agent-definition definitions "scout"))
                   (reviewer
                     (task-find-agent-definition definitions "reviewer"))
                   (librarian
                     (task-find-agent-definition definitions "librarian"))
                   (task
                     (task-find-agent-definition definitions "task"))
                   (sonic-diagnostic
                     (task-find-agent-diagnostic diagnostics "sonic"))
                   (dupe-diagnostic
                     (task-find-agent-diagnostic diagnostics "dupe")))
               (test-assert
                (and scout
                     (eq (task-agent-definition-source scout) ':project)
                     (string= (task-agent-definition-instructions scout)
                              "Project instructions."))
                "project .sexp roles override user, site, and bundled roles")
               (test-assert
                (and reviewer
                     (eq (task-agent-definition-source reviewer) ':user))
                "user .sexp roles override site and bundled roles")
               (test-assert
                (and librarian
                     (eq (task-agent-definition-source librarian) ':site)
                     (string= (task-agent-definition-instructions librarian)
                              "Research using site policy."))
                "site roles override bundled roles")
               (test-assert
                (and task
                     (eq (task-agent-definition-source task) ':bundled))
                "unclaimed roles retain their bundled definitions")
               (test-assert
                (and (null (task-find-agent-definition definitions "sonic"))
                     sonic-diagnostic
                     (eq (task-agent-definition-error-source sonic-diagnostic)
                         :project)
                     (eq (task-agent-definition-error-field sonic-diagnostic)
                         :instructions))
                "a malformed higher-precedence role blocks only its own name")
               (test-assert
                (and (task-find-agent-definition definitions "scout")
                     (task-find-agent-definition definitions "reviewer")
                     (task-find-agent-definition definitions "librarian")
                     (task-find-agent-definition definitions "task"))
                "one blocked role does not suppress unrelated definitions")
               (when
                   (= 2
                      (count-if
                       (lambda (pathname)
                         (string-equal (or (pathname-name pathname) "")
                                       "dupe"))
                       (uiop:directory-files project-directory)))
                 (test-assert
                  (and (null (task-find-agent-definition definitions "dupe"))
                       dupe-diagnostic
                       (eq (task-agent-definition-error-source dupe-diagnostic)
                           :project)
                       (eq (task-agent-definition-error-field dupe-diagnostic)
                           :name)
                       (search "same normalized role name"
                               (princ-to-string
                                (task-agent-definition-error-cause
                                 dupe-diagnostic))))
                  "case-normalized duplicate filenames fail closed before parsing")))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)
      (uiop:delete-directory-tree site-root :validate t
                                            :if-does-not-exist ':ignore)))
  nil)

(-> test-task-agents-tool () null)
(defun test-task-agents-tool ()
  "Test native role discovery, policy filtering, diagnostics, and secrecy."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root))
         (project-directory (merge-pathnames ".autolith/agents/" root))
         (hidden-broken-path
           (merge-pathnames "hidden-broken.sexp" project-directory))
         (secret
           "AUTOLITH-TASK-AGENT-INSTRUCTION-SENTINEL-71D21A")
         (registry
           (task-augment-tool-registry (make-default-tool-registry))))
    (unwind-protect
         (progn
           (task-tests--write-native-form
            (merge-pathnames "allowed.sexp" project-directory)
            (task-tests--role-form
             "allowed" "An explicitly spawnable role." secret))
           (task-tests--write-native-form
            (merge-pathnames "denied.sexp" project-directory)
            (task-tests--role-form
             "denied" "A role outside the child policy."
             "This instruction must not matter."))
           (task-tests--write-text
            (merge-pathnames "blocked.sexp" project-directory)
            "(:name \"blocked\" :description \"Missing instructions\")")
           (task-tests--write-text
            hidden-broken-path
            "(:name \"hidden-broken\" :description \"Private malformed role\" :instructions 177771)")
           (let* ((primary
                    (task-tests--primary-agent
                     configuration "agents-primary" registry))
                  (tool (tool-registry-find registry "task" "agents")))
             (test-assert tool
                          "the default registry exposes task.agents")
             (let ((orchestrator (task-agents-tool-orchestrator tool)))
               (labels
                   ((invoke (selected-tool viewer offset limit)
                      "Execute task.agents and return its result and native form."
                      (let* ((context
                               (make-instance
                                'tool-context
                                :configuration (agent-configuration viewer)
                                :worker nil
                                :conversation (agent-conversation viewer)
                                :registry (agent-tool-registry viewer)
                                :agent viewer))
                             (result
                               (tool-execute
                                selected-tool context
                                (json-object "offset" offset "limit" limit)))
                             (form
                               (task-tests--read-exact-native-value
                                (tool-result-content result))))
                        (values result form)))

                    (entry (form name kind)
                      "Return the native entry named NAME with KIND from FORM."
                      (find-if
                       (lambda (record)
                         (and (eq (getf record :kind) kind)
                              (string= (getf record :name) name)))
                       (getf (rest form) :entries)))

                    (field-present-p (record field)
                      "Return true when FIELD occurs as a key in RECORD."
                      (loop for tail on record by #'cddr
                            thereis (eq (first tail) field)))

                    (run-report (viewer selected-registry agent-name)
                      "Invoke task.run as VIEWER and return its failure report."
                      (let* ((context
                               (make-instance
                                'tool-context
                                :configuration (agent-configuration viewer)
                                :worker nil
                                :conversation (agent-conversation viewer)
                                :registry selected-registry
                                :agent viewer))
                             (result
                               (tool-registry-execute-call
                                selected-registry
                                (json-object
                                 "namespace" "task"
                                 "name" "run"
                                 "arguments"
                                 (json-encode
                                  (json-object
                                   "agent" agent-name
                                   "task" "Exercise spawn-policy secrecy.")))
                                context)))
                        (test-assert
                         (not (tool-result-success-p result))
                         "a disallowed role request fails through registry dispatch")
                        (tool-result-content result))))
                 (multiple-value-bind (result form)
                     (invoke tool primary 0 *task-agent-page-maximum*)
                   (let ((allowed (entry form "allowed" :agent))
                         (denied (entry form "denied" :agent))
                         (blocked (entry form "blocked" :diagnostic)))
                     (test-assert
                      (and (equal form (tool-result-details result))
                           (eq (first form) :task-agents)
                           allowed denied blocked
                           (eq (getf allowed :source) :project)
                           (getf allowed :pathname)
                           (eq (getf blocked :field) :instructions))
                      "primary task.agents returns exact native role and diagnostic metadata")
                     (test-assert
                      (every
                       (lambda (field) (field-present-p allowed field))
                       '(:description :source :pathname :models
                         :reasoning-effort :tools :spawns
                         :output-contract-p :blocking-p))
                      "role discovery exposes stable policy and source fields")
                     (test-assert
                      (and (not (field-present-p allowed :instructions))
                           (null (search secret (tool-result-content result))))
                      "task.agents never exposes role instructions")))
                 (multiple-value-bind (result form)
                     (invoke tool primary 0 1)
                   (declare (ignore result))
                   (test-assert
                    (and (= (getf (rest form) :offset) 0)
                         (= (getf (rest form) :count) 1)
                         (> (getf (rest form) :total) 1)
                         (= (getf (rest form) :next-offset) 1)
                         (= (length (getf (rest form) :entries)) 1))
                    "task.agents paginates native discovery without clipping forms"))
                 (let* ((definition
                          (task-agent-definition-create
                           :name "spawn-parent"
                           :description "Permit two role names."
                           :instructions "Exercise child discovery policy."
                           :tools ':all
                           :spawns '("allowed" "blocked")
                           :source ':test))
                        (job
                          (task-tests--register-job
                           orchestrator primary definition
                           :name "spawn-parent"))
                        (child-registry
                          (task-child-tool-registry
                           registry definition orchestrator 1))
                        (child
                          (task-tests--child-viewer
                           configuration job :registry child-registry))
                        (child-tool
                          (tool-registry-find child-registry "task" "agents")))
                   (test-assert
                    child-tool
                    "a child allowed to delegate inherits task.agents")
                   (multiple-value-bind (result form)
                       (invoke child-tool child 0 *task-agent-page-maximum*)
                     (let ((entries (getf (rest form) :entries)))
                       (test-assert
                        (and
                         (equal
                          (sort (mapcar (lambda (entry)
                                          (getf entry :name))
                                        entries)
                                #'string<)
                          '("allowed" "blocked"))
                         (entry form "allowed" :agent)
                         (entry form "blocked" :diagnostic)
                         (null (entry form "denied" :agent))
                         (null (search secret (tool-result-content result))))
                        "child task.agents shows only spawnable roles and reserved-name diagnostics")))
                   (let ((unknown-report
                           (run-report child child-registry
                                       "unlisted-request"))
                         (malformed-report
                           (run-report child child-registry
                                       "hidden-broken"))
                         (expected
                           "task.run failed: The current agent may not spawn the requested role."))
                     (test-assert
                      (and (string= unknown-report expected)
                           (null (search "Available agents" unknown-report))
                           (null (search "allowed" unknown-report))
                           (null (search "denied" unknown-report)))
                      "disallowed unknown roles cannot enumerate discovered roles")
                     (test-assert
                      (and (string= malformed-report expected)
                           (null
                            (search (namestring hidden-broken-path)
                                    malformed-report))
                           (null (search "hidden-broken.sexp"
                                         malformed-report))
                           (null (search "177771" malformed-report))
                           (null (search "instructions" malformed-report
                                         :test #'char-equal)))
                      "disallowed malformed roles reveal neither pathname nor parse cause")))
                 (dolist
                     (case
                      (list
                       (list
                        "no-spawn"
                        (task-agent-definition-create
                         :name "no-spawn"
                         :description "Permit no descendants."
                         :instructions "Do not delegate."
                         :spawns nil
                         :source ':test)
                        1)
                       (list
                        "max-depth"
                        (task-agent-definition-create
                         :name "max-depth"
                         :description "Reach the configured depth."
                         :instructions "Do not exceed the depth limit."
                         :spawns ':all
                         :source ':test)
                        (task-orchestrator-maximum-depth orchestrator))))
                   (destructuring-bind (name definition depth) case
                     (let* ((job
                              (task-tests--register-job
                               orchestrator primary definition :name name))
                            (child-registry
                              (task-child-tool-registry
                               registry definition orchestrator depth))
                            (child
                              (task-tests--child-viewer
                               configuration job
                               :depth depth
                               :registry child-registry)))
                       (test-assert
                        (null
                         (tool-registry-find
                          child-registry "task" "agents"))
                        (format nil
                                "~A child does not inherit task.agents"
                                name))
                       (multiple-value-bind (result form)
                           (invoke tool child 0 *task-agent-page-maximum*)
                         (declare (ignore result))
                         (test-assert
                          (and (zerop (getf (rest form) :total))
                               (null (getf (rest form) :entries)))
                          (format nil
                                  "~A child has no discoverable spawn targets"
                                  name))))))))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-tool-default-argument-types () null)
(defun test-task-tool-default-argument-types ()
  "Test that explicit JSON false and null never become omitted task defaults."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (primary
           (task-tests--primary-agent
            configuration "task-default-types" registry))
         (conversation (agent-conversation primary))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry
                          :agent primary))
         (orchestrator
           (task-run-tool-orchestrator
            (tool-registry-find registry "task" "run")))
         (definition
           (task-agent-definition-create
            :name "default-types"
            :description "Exercise explicit invalid default values."
            :instructions "Remain terminal while job.wait validates."
            :source ':test))
         (job
           (task-tests--register-job
            orchestrator primary definition :name "default-types"))
         (job-result
           (task-tests--terminal-result
            job :status ':success :output "already terminal")))
    (unwind-protect
         (progn
           (task-tests--publish-terminal job :completed job-result)
           (labels ((rejected-p (namespace name arguments)
                      "Return true when actual registry dispatch rejects ARGUMENTS."
                      (not
                       (tool-result-success-p
                        (tool-registry-execute-call
                         registry
                         (json-object "namespace" namespace
                                      "name" name
                                      "arguments" arguments)
                         context)))))
             (dolist
                 (case
                  '(("task" "agents" "{\"offset\":false}")
                    ("task" "agents" "{\"offset\":null}")
                    ("task" "agents" "{\"limit\":false}")
                    ("task" "agents" "{\"limit\":null}")
                    ("job" "list" "{\"offset\":false}")
                    ("job" "list" "{\"offset\":null}")
                    ("job" "list" "{\"limit\":false}")
                    ("job" "list" "{\"limit\":null}")))
               (destructuring-bind (namespace name arguments) case
                 (test-assert
                  (rejected-p namespace name arguments)
                  (format nil
                          "~A.~A rejects explicit non-integer default input ~A"
                          namespace name arguments))))
             (dolist (value '("false" "null"))
               (test-assert
                (rejected-p
                 "job" "wait"
                 (format nil
                         "{\"id\":~A,\"timeout-seconds\":~A}"
                         (json-encode (job-identifier job))
                         value))
                (format nil
                        "job.wait rejects explicit timeout-seconds ~A"
                        value)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-native-output-contracts () null)
(defun test-task-native-output-contracts ()
  "Test recursive native output schemas and exact JSON boundary conversion."
  (let* ((schema
           (task-output-schema-normalize
            '(:type :object
              :properties
              (("enabled" (:type :boolean))
               ("nothing" (:type :null))
               ("items"
                (:type :array
                 :items
                 (:type :object
                  :properties
                  (("name" (:type :string))
                   ("score" (:type :number)))
                  :required ("name")
                  :additional-properties nil)
                 :min-items 1
                 :max-items 2)))
              :required ("enabled" "nothing" "items")
              :additional-properties nil)
            :source ':programmatic
            :definition-name "recursive"))
         (provider-schema (task-output-schema->json schema))
         (candidate
           (task-json-decode
            "{\"enabled\":false,\"nothing\":null,\"items\":[{\"name\":\"one\",\"score\":1.5}]}")))
    (test-assert (task-output-schema-valid-p candidate schema)
                 "recursive provider JSON satisfies its native output DSL")
    (test-assert
     (and (string= (json-get provider-schema "type") "object")
          (eq (json-get provider-schema "additionalProperties") false)
          (vectorp (json-get provider-schema "required"))
          (string=
           (json-get
            (json-get
             (json-get (json-get provider-schema "properties") "items")
             "items")
            "type")
           "object"))
     "native recursive schemas convert to JSON only at the provider boundary")
    (test-assert
     (not
      (task-output-schema-valid-p
       (task-json-decode
        "{\"enabled\":null,\"nothing\":false,\"items\":[{\"name\":\"one\"}]}")
       schema))
     "recursive validation keeps JSON false distinct from JSON null")
    (test-assert
     (not
      (task-output-schema-valid-p
       (task-json-decode
        "{\"enabled\":false,\"nothing\":null,\"items\":[{\"name\":\"one\",\"extra\":1}]}")
       schema))
     "recursive validation enforces nested additional-property policy"))
  (let* ((enum-schema
           (task-output-schema-normalize
            '(:enum (nil :null t))
            :source ':programmatic
            :definition-name "enum"))
         (provider-enum
           (json-get (task-output-schema->json enum-schema) "enum")))
    (test-assert
     (and (= (length provider-enum) 3)
          (eq (aref provider-enum 0) false)
          (eq (aref provider-enum 1) :null)
          (eq (aref provider-enum 2) t))
     "native NIL and :NULL become distinct JSON false and null enum values"))
  (dolist
      (case
       '(((:type :array) :items)
         ((:type :object
           :properties (("known" (:type :string)))
           :required ("missing"))
          :required)
         ((:type :object
           :properties
           (("same" (:type :string))
            ("same" (:type :integer))))
          :properties)
         ((:type :boolean :enum (nil :null)) :enum)))
    (destructuring-bind (invalid-schema expected-field) case
      (let ((condition
              (handler-case
                  (progn
                    (task-output-schema-normalize
                     invalid-schema
                     :source ':programmatic
                     :definition-name "invalid-output")
                    nil)
                (task-agent-definition-error (error)
                  error))))
        (test-assert
         (and condition
              (eq (task-agent-definition-error-field condition)
                  expected-field)
              (eq (task-agent-definition-error-source condition)
                  :programmatic)
              (string=
               (task-agent-definition-error-definition-name condition)
               "invalid-output"))
         (format nil "invalid recursive output field ~S has a typed diagnostic"
                 expected-field)))))
  (let* ((provider-value
           (task-json-decode
            "{\"z\":null,\"a\":[false,true,{\"quote\":\"a\\\"b\\n\"}],\"n\":2.5}"))
         (native-value (task-json->sexp provider-value))
         (entries (rest native-value))
         (array (second (assoc "a" entries :test #'string=))))
    (test-assert
     (and (eq (first native-value) :object)
          (equal (mapcar #'first entries) '("a" "n" "z"))
          (eq (first array) :array)
          (null (second array))
          (eq (third array) t)
          (eq (second (assoc "z" entries :test #'string=)) :null))
     "provider JSON becomes sorted tagged readable s-expression data")
    (test-assert
     (equal native-value
            (task-json->sexp (task-sexp->json native-value)))
     "tagged objects, arrays, false, null, numbers, and escaped strings round trip")
    (test-assert
     (handler-case
         (progn
           (task-sexp->json
            '(:object ("duplicate" 1) ("duplicate" 2)))
           nil)
       (task-error ()
         t))
     "tagged native objects reject duplicate keys during reconstruction")
    (test-assert
     (handler-case
         (progn
           (task-sexp->json '("untagged" nil :null))
           nil)
       (task-error ()
         t))
     "untagged lists cannot cross the durable task result boundary")
    (test-assert
     (handler-case
         (progn
           (task-json-decode
            "{\"complete\":true} false"
            :tool-name "yield.submit")
           nil)
       (task-error (condition)
         (string= (tool-error-tool-name condition) "yield.submit")))
     "task JSON decoding rejects trailing values with canonical tool metadata"))
  nil)

(-> test-task-yield-contract () null)
(defun test-task-yield-contract ()
  "Test exact yield semantics through provider JSON argument decoding."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((definition
                    (task-agent-definition-create
                     :name "boolean-output"
                     :description "Return a boolean."
                     :instructions "Yield one explicit boolean."
                     :output '(:type :boolean)
                     :source ':test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-false"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"text\":\"false value\",\"data\":false}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-called-p completion)
                   (eq (task-completion-status completion) :success)
                   (task-completion-data-present-p completion)
                   (eq (task-completion-data completion) false)
                   (null
                    (task-json->sexp
                     (task-completion-data completion))))
              "registry decoding preserves an explicitly supplied JSON false")
             (let ((durable
                     (task--assemble-child-result
                      (getf fixture :job)
                      (agent-test-result "yield-false-result" nil)
                      (getf fixture :child)
                      (getf fixture :conversation)
                      completion)))
               (test-assert
                (and (getf durable :structured-output-present-p)
                     (task--plist-key-present-p durable :structured-output)
                     (null (getf durable :structured-output)))
                "durable task results tag false as NIL with an explicit presence bit"))
             (test-assert
              (not
               (tool-result-success-p
                (task-tests--execute-yield
                 fixture
                 "{\"status\":\"success\",\"data\":true}")))
              "yield.submit rejects every call after the exact terminal yield"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "null-output"
                     :description "Return null."
                     :instructions "Yield one explicit null."
                     :output '(:type :null)
                     :source ':test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-null"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"data\":null}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-data-present-p completion)
                   (eq (task-completion-data completion) :null)
                   (eq (task-json->sexp
                        (task-completion-data completion))
                       :null))
              "registry decoding preserves JSON null separately from false"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "optional-output"
                     :description "Return optional data."
                     :instructions "Yield a concise result."
                     :source ':test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-absent"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"text\":\"no structured data\"}")))
             (test-assert
              (and (tool-result-success-p result)
                   (not (task-completion-data-present-p completion))
                   (null (task-completion-data completion)))
              "absent yield data remains distinct from explicit false"))
           (dolist
               (case
                '(("required-missing"
                   (:type :boolean)
                   "{\"status\":\"success\"}")
                  ("success-error"
                   nil
                   "{\"status\":\"success\",\"error\":\"impossible\"}")
                  ("success-empty"
                   nil
                   "{\"status\":\"success\",\"text\":\" \\t\\n \"}")
                  ("unknown-field"
                   nil
                   "{\"status\":\"success\",\"text\":\"done\",\"legacy\":true}")
                  ("failed-with-data"
                   nil
                   "{\"status\":\"failed\",\"error\":\"blocked\",\"data\":false}")
                  ("failed-empty-error"
                   nil
                   "{\"status\":\"failed\",\"error\":\"\"}")
                  ("failed-blank-error"
                   nil
                   "{\"status\":\"failed\",\"error\":\" \\t \"}")
                  ("aborted-no-error"
                   nil
                   "{\"status\":\"aborted\"}")
                  ("status-case"
                   nil
                   "{\"status\":\"Success\"}")
                  ("non-string-text"
                   nil
                   "{\"status\":\"success\",\"text\":null}")))
             (destructuring-bind (name output arguments) case
               (let* ((definition
                        (task-agent-definition-create
                         :name name
                         :description "Exercise one invalid terminal yield."
                         :instructions "Follow the exact yield contract."
                         :output output
                         :source ':test))
                      (fixture
                        (task-tests--yield-fixture
                         configuration definition name))
                      (result
                        (task-tests--execute-yield fixture arguments)))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (not
                        (task-completion-called-p
                         (getf fixture :completion))))
                  (format nil "yield contract rejects ~A without terminal mutation"
                          name)))))
           (let* ((definition
                    (task-agent-definition-create
                     :name "bounded-label"
                     :description "Exercise the terminal label bound."
                     :instructions "Yield one bounded label."
                     :source ':test))
                  (oversized-fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-oversized-label"))
                  (oversized-result
                    (task-tests--execute-yield
                     oversized-fixture
                     (json-encode
                      (json-object
                       "status" "success"
                       "text" "done"
                       "label"
                       (make-string
                        (1+ *task-result-label-maximum-characters*)
                        :initial-element #\L))))))
             (test-assert
              (and (not (tool-result-success-p oversized-result))
                   (not
                    (task-completion-called-p
                     (getf oversized-fixture :completion))))
              "yield.submit rejects labels beyond the terminal retention bound")
             (let* ((fixture
                      (task-tests--yield-fixture
                       configuration definition "yield-bounded-label"))
                    (label
                      (make-string
                       *task-result-label-maximum-characters*
                       :initial-element #\L))
                    (result
                      (task-tests--execute-yield
                       fixture
                       (json-encode
                        (json-object "status" "success"
                                     "text" "done"
                                     "label" label))))
                    (durable
                      (task--assemble-child-result
                       (getf fixture :job)
                       (agent-test-result "bounded-label-result" nil)
                       (getf fixture :child)
                       (getf fixture :conversation)
                       (getf fixture :completion))))
               (test-assert
                (and (tool-result-success-p result)
                     (task-tests--publish-terminal
                      (getf fixture :job) :completed durable)
                     (string=
                      (getf (job-result (getf fixture :job)) :label)
                      label))
                "a maximum-length yield label survives terminal compaction")))
           (let* ((definition
                    (task-agent-definition-create
                     :name "failed-result"
                     :description "Report a failure."
                     :instructions "Yield one explained failure."
                     :source ':test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-failed"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"failed\",\"error\":\"dependency unavailable\"}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-called-p completion)
                   (eq (task-completion-status completion) :failed)
                   (string= (task-completion-error completion)
                            "dependency unavailable")
                   (not (task-completion-data-present-p completion)))
              "an explained failed yield is an accepted terminal result")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)


(-> test-task-child-steering-mailbox () null)
(defun test-task-child-steering-mailbox ()
  "Test bounded child steering, terminal claims, and durable safe-boundary delivery."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "steering-child"
            :description "Exercise child steering."
            :instructions "Accept steering before yielding."
            :source ':test)))
    (labels ((fixture (identifier &key (state ':running))
               (task-tests--yield-fixture
                configuration definition identifier :state state))

             (mark-running (job)
               (with-lock-held ((cl-jobpond::job--lock job))
                 (setf (job-state job) ':running))
               (task-job--set-progress-state job ':running)
               job)

             (steering-texts (entries)
               (mapcar
                (lambda (entry)
                  (user-message-input-text
                   (agent-steering-input-content entry)))
                entries))

             (valid-yield (fixture)
               (task-tests--execute-yield
                fixture
                "{\"status\":\"success\",\"text\":\"done\"}"))

             (run-claim-race (index claim-kind)
               (let* ((fixture
                        (fixture
                         (format nil "steering-~(~A~)-race-~D"
                                 claim-kind index)))
                      (job (mark-running (getf fixture :job)))
                      (normal-result
                        (agent-test-result
                         "normal-race"
                         (list (agent-test-message "done"))
                         :turn-completion ':end))
                      (gate-lock (make-lock "Autolith steering race gate"))
                      (gate-condition (make-condition-variable))
                      (ready-count 0)
                      (released-p nil)
                      (enqueue-reason nil)
                      (enqueue-condition nil)
                      (claim-result nil)
                      (claim-condition nil)
                      (enqueue-thread nil)
                      (claim-thread nil))
                 (labels ((await-release ()
                            (with-lock-held (gate-lock)
                              (incf ready-count)
                              (task--condition-broadcast gate-condition)
                              (loop until released-p
                                    unless
                                    (condition-wait
                                     gate-condition gate-lock :timeout 2)
                                      do (error
                                          "Timed out waiting for the steering race release.")))))
                   (unwind-protect
                        (progn
                          (setf enqueue-thread
                                (make-thread
                                 (lambda ()
                                   (handler-case
                                       (progn
                                         (await-release)
                                         (multiple-value-bind (entry reason)
                                             (task-job-enqueue-steering
                                              job "race context")
                                           (declare (ignore entry))
                                           (setf enqueue-reason reason)))
                                     (condition (condition)
                                       (setf enqueue-condition condition))))
                                 :name "Autolith steering enqueue race")
                                claim-thread
                                (make-thread
                                 (lambda ()
                                   (handler-case
                                       (progn
                                         (await-release)
                                         (setf claim-result
                                               (ecase claim-kind
                                                 (:yield
                                                  (valid-yield fixture))
                                                 (:normal
                                                  (agent-turn-complete-p
                                                   (getf fixture :child)
                                                   normal-result)))))
                                     (condition (condition)
                                       (setf claim-condition condition))))
                                 :name "Autolith steering completion race"))
                          (with-lock-held (gate-lock)
                            (loop until (= ready-count 2)
                                  unless
                                  (condition-wait
                                   gate-condition gate-lock :timeout 2)
                                    do (error
                                        "Timed out synchronizing the steering race."))
                            (setf released-p t)
                            (task--condition-broadcast gate-condition))
                          (join-thread enqueue-thread)
                          (setf enqueue-thread nil)
                          (join-thread claim-thread)
                          (setf claim-thread nil)
                          (let ((enqueue-won-p
                                  (eq enqueue-reason ':accepted))
                                (claim-won-p
                                  (ecase claim-kind
                                    (:yield
                                     (and claim-result
                                          (tool-result-success-p claim-result)))
                                    (:normal claim-result))))
                            (test-assert
                             (and (null enqueue-condition)
                                  (null claim-condition)
                                  (not (eq enqueue-won-p claim-won-p))
                                  (if enqueue-won-p
                                      (and (eq enqueue-reason ':accepted)
                                           (not
                                            (task-completion-called-p
                                             (getf fixture :completion)))
                                           (= (task-job-steering-pending-count job)
                                              1))
                                      (and (eq enqueue-reason ':closed)
                                           (ecase claim-kind
                                             (:yield
                                              (task-completion-called-p
                                               (getf fixture :completion)))
                                             (:normal
                                              (and
                                               (not
                                                (task-completion-called-p
                                                 (getf fixture :completion)))
                                               (task-job-steering-closed-p job))))
                                           (zerop
                                            (task-job-steering-pending-count
                                             job)))))
                             (format nil
                                     "enqueue and ~(~A~) completion share one atomic winner"
                                     claim-kind))))
                     (with-lock-held (gate-lock)
                       (setf released-p t)
                       (task--condition-broadcast gate-condition))
                     (when enqueue-thread
                       (join-thread enqueue-thread))
                     (when claim-thread
                       (join-thread claim-thread))
                     (task-job-close-steering job))))))
      (unwind-protect
           (progn
             (let* ((fixture (fixture "steering-queued" :state ':queued))
                    (job (getf fixture :job)))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "not yet")
                 (test-assert
                  (and (null entry)
                       (eq reason ':not-running)
                       (zerop (task-job-steering-pending-count job)))
                  "queued children reject steering without retaining it")))
             (let* ((fixture (fixture "steering-fifo"))
                    (job (mark-running (getf fixture :job))))
               (multiple-value-bind (first first-reason)
                   (task-job-enqueue-steering job "first")
                 (multiple-value-bind (second second-reason)
                     (task-job-enqueue-steering job "second")
                   (test-assert
                    (and (eq first-reason ':accepted)
                         (eq second-reason ':accepted)
                         (not
                          (string=
                           (agent-steering-input-identifier first)
                           (agent-steering-input-identifier second)))
                         (= (getf (task-job-snapshot job)
                                  :pending-prompt-count)
                            2)
                         (= (getf (task-job-live-activity job)
                                  :pending-prompt-count)
                            2))
                    "running children expose accepted prompt counts")
                   (let ((entries (task-job-take-steering job)))
                     (test-assert
                      (and (equal (steering-texts entries)
                                  '("first" "second"))
                           (= (task-job-steering-pending-count job) 2)
                           (task-job-acknowledge-steering
                            job (agent-steering-input-identifier first))
                           (= (task-job-steering-pending-count job) 1)
                           (not
                            (task-job-acknowledge-steering job "missing"))
                           (task-job-acknowledge-steering
                            job (agent-steering-input-identifier second))
                           (zerop (task-job-steering-pending-count job)))
                      "child steering drains FIFO and remains retained until acknowledgment")))
               (test-assert
                (task-job--claim-normal-completion job)
                "an empty mailbox permits normal completion")
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "too late")
                 (test-assert
                  (and (null entry) (eq reason ':closed))
                  "normal completion closes later steering admission")))
             (let* ((fixture (fixture "steering-bounds"))
                    (job (mark-running (getf fixture :job))))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "")
                 (test-assert
                  (and (null entry) (eq reason ':empty))
                  "child steering rejects empty content"))
               (let ((*task-steering-maximum-characters* 3))
                 (multiple-value-bind (entry reason)
                     (task-job-enqueue-steering job "four")
                   (test-assert
                    (and (null entry) (eq reason ':content-too-large))
                    "child steering enforces its per-message bound")))
                (let ((*task-steering-maximum-items* 3)
                      (*task-steering-maximum-characters* 4)
                      (*task-steering-maximum-total-characters* 5))
                  (multiple-value-bind (accepted accepted-reason)
                      (task-job-enqueue-steering job "1234")
                    (task-job-take-steering job)
                    (multiple-value-bind (entry reason)
                        (task-job-enqueue-steering job "12")
                      (test-assert
                       (and accepted
                            (eq accepted-reason ':accepted)
                            (null entry)
                            (eq reason ':full)
                            (= (task-job-steering-pending-count job) 1)
                            (task-job-acknowledge-steering
                             job (agent-steering-input-identifier accepted))
                            (zerop (task-job-steering-pending-count job)))
                       "character rejection retains accepted in-flight steering")))
                (task-job-close-steering job)))
              (let* ((fixture (fixture "steering-count-bound"))
                     (job (mark-running (getf fixture :job))))
                (let ((*task-steering-maximum-items* 2)
                      (*task-steering-maximum-characters* 4)
                      (*task-steering-maximum-total-characters* 20))
                  (multiple-value-bind (first first-reason)
                      (task-job-enqueue-steering job "one")
                    (task-job-take-steering job)
                    (multiple-value-bind (second second-reason)
                        (task-job-enqueue-steering job "two")
                      (multiple-value-bind (entry reason)
                          (task-job-enqueue-steering job "more")
                        (test-assert
                         (and first
                              (eq first-reason ':accepted)
                              second
                              (eq second-reason ':accepted)
                              (null entry)
                              (eq reason ':full)
                              (= (task-job-steering-pending-count job) 2)
                              (equal (steering-texts (task-job-take-steering job))
                                     '("two"))
                              (task-job-acknowledge-steering
                               job (agent-steering-input-identifier first))
                              (task-job-acknowledge-steering
                               job (agent-steering-input-identifier second))
                              (zerop (task-job-steering-pending-count job)))
                         "count rejection retains queued and in-flight steering")))))
                (task-job-close-steering job))
             (let* ((fixture (fixture "steering-yield"))
                    (job (mark-running (getf fixture :job))))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "context before yield")
                 (declare (ignore reason))
                 (let ((queued-result (valid-yield fixture)))
                   (test-assert
                    (and (not (tool-result-success-p queued-result))
                         (not
                          (task-completion-called-p
                           (getf fixture :completion)))
                         (= (task-job-steering-pending-count job) 1))
                    "queued steering rejects terminal yield without mutation"))
                 (task-job-take-steering job)
                 (let ((in-flight-result (valid-yield fixture)))
                   (test-assert
                    (and (not (tool-result-success-p in-flight-result))
                         (not
                          (task-completion-called-p
                           (getf fixture :completion))))
                    "unacknowledged steering still rejects terminal yield"))
                 (task-job-acknowledge-steering
                  job (agent-steering-input-identifier entry))
                 (let ((accepted-result (valid-yield fixture)))
                   (test-assert
                    (and (tool-result-success-p accepted-result)
                         (task-completion-called-p
                          (getf fixture :completion)))
                    "yield succeeds after every steering append is durable"))
                 (multiple-value-bind (late-entry late-reason)
                     (task-job-enqueue-steering job "after yield")
                   (test-assert
                    (and (null late-entry) (eq late-reason ':closed))
                    "accepted yield rejects every later steering message"))))
             (let* ((fixture (fixture "steering-normal-stop"))
                    (job (mark-running (getf fixture :job)))
                    (child (getf fixture :child))
                    (result
                      (agent-test-result
                       "normal-stop" (list (agent-test-message "done"))
                       :turn-completion ':end)))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "continue instead")
                 (declare (ignore reason))
                 (test-assert
                  (not (agent-turn-complete-p child result))
                  "pending steering prevents a normal provider stop")
                 (task-job-take-steering job)
                 (task-job-acknowledge-steering
                  job (agent-steering-input-identifier entry))
                 (test-assert
                  (and (agent-turn-complete-p child result)
                       (task-job-steering-closed-p job))
                  "normal completion atomically claims an empty mailbox")))
             (let* ((fixture (fixture "steering-response-promotion"))
                    (job (mark-running (getf fixture :job)))
                    (orchestrator (task-job-orchestrator job))
                    (events nil)
                    (listener
                      (lambda (channel payload)
                        (when (eq channel ':task-subagent-verbal-response)
                          (push (copy-tree payload) events)))))
               (unwind-protect
                    (progn
                      (task-orchestrator-add-listener orchestrator listener)
                      (multiple-value-bind (entry reason)
                          (task-job-enqueue-steering job "background context")
                        (test-assert
                         (and entry
                              (eq reason ':accepted)
                              (zerop
                               (task-job-response-promotion-pending-count job)))
                         "ordinary child steering creates no response promotion token")
                        (task-job-take-steering job)
                        (task-job-acknowledge-steering
                         job (agent-steering-input-identifier entry)))
                      (multiple-value-bind (first first-reason)
                          (task-job-enqueue-steering
                           job "first promoted prompt" :promote-response-p t)
                        (multiple-value-bind (second second-reason)
                            (task-job-enqueue-steering
                             job "second promoted prompt" :promote-response-p t)
                          (let ((first-time (get-universal-time))
                                (second-time (1+ (get-universal-time))))
                            (test-assert
                             (and (eq first-reason ':accepted)
                                  (eq second-reason ':accepted)
                                  (= (task-job-response-promotion-pending-count job)
                                     2))
                             "promoted steering reserves one FIFO token per accepted prompt")
                            (task-job-note-agent-status
                             job ':provider-request-started
                             (list :request-number 1))
                            (task-job-note-agent-status
                             job ':tool-call-completed
                             (list :tool "resource.read"))
                            (task-job-note-agent-status
                             job ':assistant-response-persisted
                             (list :text "   " :time first-time))
                            (test-assert
                             (and (null events)
                                  (= (task-job-response-promotion-pending-count job)
                                     2))
                             "tools, status updates, and blank text do not consume promotion tokens")
                            (task-job-note-agent-status
                             job ':assistant-response-persisted
                             (list :text "first answer" :time first-time))
                            (test-assert
                             (and (= (length events) 1)
                                  (= (task-job-response-promotion-pending-count job)
                                     1))
                             "one durable verbal response consumes exactly one token and emits once")
                            (task-job-note-agent-status
                             job ':assistant-response-persisted
                             (list :text "second answer" :time second-time))
                            (task-job-note-agent-status
                             job ':assistant-response-persisted
                             (list :text "unsteered answer" :time (1+ second-time)))
                            (let* ((ordered-events (nreverse events))
                                   (first-event (first ordered-events))
                                   (second-event (second ordered-events)))
                              (test-assert
                               (and (= (length ordered-events) 2)
                                    (zerop
                                     (task-job-response-promotion-pending-count
                                      job))
                                    (string=
                                     (getf first-event :id)
                                     (session-job-identifier job))
                                    (string=
                                     (getf first-event :execution-id)
                                     (task-job-execution-identifier job))
                                    (string=
                                     (getf first-event :child-name)
                                     (task-job-display-name job))
                                    (string=
                                     (getf first-event :steering-id)
                                     (agent-steering-input-identifier first))
                                    (string= (getf first-event :text)
                                             "first answer")
                                    (= (getf first-event :time) first-time)
                                    (string=
                                     (getf second-event :steering-id)
                                     (agent-steering-input-identifier second))
                                    (string= (getf second-event :text)
                                             "second answer")
                                    (= (getf second-event :time) second-time))
                               "promoted responses preserve FIFO steering and portable child identity"))))))
                 (task-orchestrator-remove-listener orchestrator listener)
                 (task-job-close-steering job)))
             (let* ((fixture (fixture "steering-response-bound"))
                    (job (mark-running (getf fixture :job))))
               (unwind-protect
                    (let ((*task-response-promotion-maximum-items* 1))
                      (multiple-value-bind (first first-reason)
                          (task-job-enqueue-steering
                           job "reserve one response" :promote-response-p t)
                        (test-assert
                         (and first (eq first-reason ':accepted))
                         "the response promotion bound accepts its first token")
                        (task-job-take-steering job)
                        (task-job-acknowledge-steering
                         job (agent-steering-input-identifier first))
                        (multiple-value-bind (second second-reason)
                            (task-job-enqueue-steering
                             job "reserve another response" :promote-response-p t)
                          (test-assert
                           (and (null second)
                                (eq second-reason ':full)
                                (= (task-job-response-promotion-pending-count job)
                                   1)
                                (zerop (task-job-steering-pending-count job)))
                           "the response token bound rejects promoted steering atomically"))
                        (multiple-value-bind (ordinary ordinary-reason)
                            (task-job-enqueue-steering job "ordinary still fits")
                          (test-assert
                           (and ordinary
                                (eq ordinary-reason ':accepted)
                                (= (task-job-response-promotion-pending-count job)
                                   1)
                                (= (task-job-close-steering job) 1)
                                (zerop
                                 (task-job-response-promotion-pending-count job)))
                           "ordinary steering bypasses the token bound and close clears tokens once"))))
                 (task-job-close-steering job)))
             (let* ((fixture (fixture "steering-response-race"))
                    (job (mark-running (getf fixture :job)))
                    (orchestrator (task-job-orchestrator job))
                    (gate-lock (make-lock "Autolith response promotion race gate"))
                    (gate-condition (make-condition-variable))
                    (event-lock (make-lock "Autolith response promotion race events"))
                    (ready-count 0)
                    (released-p nil)
                    (results (make-array 8 :initial-element nil))
                    (events nil)
                    (threads nil)
                    (listener
                      (lambda (channel payload)
                        (when (eq channel ':task-subagent-verbal-response)
                          (with-lock-held (event-lock)
                            (push (copy-tree payload) events))))))
               (labels ((await-release ()
                          (with-lock-held (gate-lock)
                            (incf ready-count)
                            (task--condition-broadcast gate-condition)
                            (loop until released-p
                                  unless
                                  (condition-wait
                                   gate-condition gate-lock :timeout 2)
                                    do (error
                                        "Timed out waiting for the response race release.")))))
                 (unwind-protect
                      (progn
                        (task-orchestrator-add-listener orchestrator listener)
                        (multiple-value-bind (entry reason)
                            (task-job-enqueue-steering
                             job "race response" :promote-response-p t)
                          (test-assert
                           (and entry (eq reason ':accepted))
                           "the response race reserves one promotion token")
                          (task-job-take-steering job)
                          (task-job-acknowledge-steering
                           job (agent-steering-input-identifier entry))
                          (dotimes (index (length results))
                            (let ((thread-index index))
                              (push
                               (make-thread
                                (lambda ()
                                  (await-release)
                                  (setf
                                   (aref results thread-index)
                                   (task-job-note-verbal-response
                                    job
                                    (format nil "race answer ~D" thread-index)
                                    (get-universal-time))))
                                :name "Autolith response promotion race")
                               threads)))
                          (with-lock-held (gate-lock)
                            (loop until (= ready-count (length results))
                                  unless
                                  (condition-wait
                                   gate-condition gate-lock :timeout 2)
                                    do (error
                                        "Timed out synchronizing the response race."))
                            (setf released-p t)
                            (task--condition-broadcast gate-condition))
                          (dolist (thread threads)
                            (join-thread thread))
                          (setf threads nil)
                          (let* ((winner-index (position t results))
                                 (observed-events
                                   (with-lock-held (event-lock)
                                     (copy-list events)))
                                 (event (first observed-events)))
                            (test-assert
                             (and (= (count t results) 1)
                                  winner-index
                                  (= (length observed-events) 1)
                                  (zerop
                                   (task-job-response-promotion-pending-count
                                    job))
                                  (string=
                                   (getf event :steering-id)
                                   (agent-steering-input-identifier entry))
                                  (string=
                                   (getf event :text)
                                   (format nil "race answer ~D" winner-index)))
                             "concurrent verbal notifications consume one token exactly once"))))
                   (with-lock-held (gate-lock)
                     (setf released-p t)
                     (task--condition-broadcast gate-condition))
                   (dolist (thread threads)
                     (ignore-errors (join-thread thread)))
                   (task-orchestrator-remove-listener orchestrator listener)
                   (task-job-close-steering job))))
             (dotimes (index 12)
               (run-claim-race index ':yield)
               (run-claim-race index ':normal))
             (let* ((fixture (fixture "steering-tool-free"))
                    (job (mark-running (getf fixture :job)))
                    (orchestrator (task-job-orchestrator job))
                    (base-child (getf fixture :child))
                    (conversation (getf fixture :conversation))
                    (reasoning-item
                      (json-object
                       "type" "reasoning"
                       "summary"
                       (json-array
                        (json-object
                         "type" "summary_text"
                         "text" "before context"))))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (list
                        (agent-test-result
                         "before-steering"
                         (list reasoning-item)
                         :turn-completion ':end)
                        (agent-test-result
                         "after-steering"
                         (list (agent-test-message "after context"))
                         :turn-completion ':end))))
                    (child
                      (make-instance
                       'task-child-agent
                       :configuration (agent-configuration base-child)
                       :provider provider
                       :conversation conversation
                       :tool-registry (agent-tool-registry base-child)
                       :worker nil
                       :definition (task-child-agent-definition base-child)
                       :identity (task-child-agent-identity base-child)
                       :depth (task-child-agent-depth base-child)
                       :completion (task-child-agent-completion base-child)
                       :orchestrator (task-child-agent-orchestrator base-child)
                       :job job))
                    (events nil)
                    (listener
                      (lambda (channel payload)
                        (when (eq channel ':task-subagent-verbal-response)
                          (push (copy-tree payload) events))))
                    (observer
                      (callback-agent-observer-create
                       :status-callback
                       (lambda (status details)
                         (task-job-note-agent-status job status details))
                       :steering-callback
                       (lambda ()
                         (task-job-take-steering job))
                       :steering-persisted-callback
                       (lambda (identifier)
                         (task-job-acknowledge-steering job identifier)))))
               (unwind-protect
                    (progn
                      (task-orchestrator-add-listener orchestrator listener)
                      (multiple-value-bind (steering reason)
                          (task-job-enqueue-steering
                           job "tool-free steering" :promote-response-p t)
                        (test-assert
                         (and steering (eq reason ':accepted))
                         "the tool-free child accepts promoted steering")
                        (let ((result
                                (agent-run-user-turn
                                 child "initial child input" :observer observer)))
                          (test-assert
                           (and
                            (string=
                             (provider-result-response-id result)
                             "after-steering")
                            (equal
                             (nreverse
                              (scripted-provider-input-counts provider))
                             '(1 3))
                            (zerop (task-job-steering-pending-count job))
                            (zerop
                             (task-job-response-promotion-pending-count job)))
                           "tool-free steering is durable before its follow-up response"))
                        (let ((event (first events)))
                          (test-assert
                           (and (= (length events) 1)
                                (string= (getf event :id)
                                         (session-job-identifier job))
                                (string=
                                 (getf event :steering-id)
                                 (agent-steering-input-identifier steering))
                                (string= (getf event :text) "after context")
                                (typep (getf event :time) 'timestamp))
                           "the durable agent boundary emits one steered verbal response after reasoning")))
                      (let ((user-records
                              (loop for record in
                                      (rest
                                       (conversation--read-records
                                        (conversation-pathname conversation)))
                                    when
                                    (and (eq (first record) ':message)
                                         (eq (getf (rest record) :role) ':user))
                                      collect record)))
                        (test-assert
                         (equal
                          (mapcar
                           (lambda (record)
                             (getf (rest record) :content))
                           user-records)
                          '("initial child input" "tool-free steering"))
                         "tool-free child steering is durable ordinary user input")))
                 (task-orchestrator-remove-listener orchestrator listener)
                 (task-job-close-steering job)))
             (let* ((fixture (fixture "steering-cancellation"))
                    (job (mark-running (getf fixture :job))))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "accepted before cancellation")
                 (test-assert
                  (and entry (eq reason ':accepted))
                  "running child accepts steering before cancellation")
                 (test-assert
                  (job-cancel job :reason ':steering-test)
                  "running child cancellation records its first reason")
                 (multiple-value-bind (late-entry late-reason)
                     (task-job-enqueue-steering job "after cancellation")
                   (test-assert
                    (and (null late-entry) (eq late-reason ':closing))
                    "cancelled child rejects later steering before publication"))
                 (test-assert
                  (task-tests--publish-terminal
                   job ':completed
                   (list :status ':success :output "must be discarded"))
                  "cancelled child publishes one terminal record")
                 (let* ((final-result (job-result job))
                        (artifact
                          (task-tests--read-exact-native-value
                           (uiop:read-file-string
                            (getf final-result :output-path)))))
                   (test-assert
                    (and (eq (job-state job) ':aborted)
                         (= (getf final-result :undelivered-prompt-count) 1)
                         (= (getf artifact :undelivered-prompt-count) 1)
                         (zerop (task-job-steering-pending-count job))
                         (task-job-steering-closed-p job))
                    "cancellation publication records accepted undelivered steering"))))
             (let* ((fixture (fixture "steering-cancelled-claims"))
                    (job (mark-running (getf fixture :job)))
                    (child (getf fixture :child))
                    (normal-result
                      (agent-test-result
                       "cancelled-normal-stop"
                       (list (agent-test-message "done"))
                       :turn-completion ':end)))
               (test-assert
                (job-cancel job :reason ':steering-claim-test)
                "cancellation precedes the terminal claim checks")
               (let ((yield-result (valid-yield fixture)))
                 (test-assert
                  (and (not (tool-result-success-p yield-result))
                       (not
                        (task-completion-called-p
                         (getf fixture :completion)))
                       (not (task-job-steering-closed-p job)))
                  "cancelled child rejects terminal yield without closing its mailbox"))
               (test-assert
                (and (not (agent-turn-complete-p child normal-result))
                     (not (task-job-steering-closed-p job)))
                "cancelled child rejects normal completion without closing its mailbox")
               (test-assert
                (task-tests--publish-terminal job ':completed nil)
                "cancelled terminal-claim fixture publishes its aborted result"))
             (let* ((fixture (fixture "steering-publication-claim"))
                    (job (mark-running (getf fixture :job)))
                    (child (getf fixture :child))
                    (normal-result
                      (agent-test-result
                       "publication-normal-stop"
                       (list (agent-test-message "done"))
                       :turn-completion ':end))
                    (original-hook
                      (cl-jobpond:job-terminal-result-function job))
                    (gate-lock
                      (make-lock "Autolith steering publication gate"))
                    (gate-condition (make-condition-variable))
                    (entered-p nil)
                    (released-p nil)
                    (publication-result nil)
                    (publication-condition nil)
                    (publication-thread nil))
               (unwind-protect
                    (progn
                      (setf (slot-value
                             job 'cl-jobpond::terminal-result-function)
                            (lambda (hook-job state result report)
                              (with-lock-held (gate-lock)
                                (setf entered-p t)
                                (task--condition-broadcast gate-condition)
                                (loop until released-p
                                      unless
                                      (condition-wait
                                       gate-condition gate-lock :timeout 2)
                                        do (error
                                            "Timed out holding the publication claim.")))
                              (funcall original-hook
                                       hook-job state result report)))
                      (setf publication-thread
                            (make-thread
                             (lambda ()
                               (handler-case
                                   (setf publication-result
                                         (task-tests--publish-terminal
                                          job
                                          ':failed
                                          (list :id (job-identifier job)
                                                :name (task-job-display-name job)
                                                :agent (task-job-agent-name job)
                                                :assignment
                                                "Exercise publication admission."
                                                :status ':failed
                                                :error "stopped")))
                                 (condition (condition)
                                   (setf publication-condition condition))))
                             :name "Autolith steering publication claim"))
                      (with-lock-held (gate-lock)
                        (loop until entered-p
                              unless
                              (condition-wait
                               gate-condition gate-lock :timeout 2)
                                do (error
                                    "Timed out waiting for the publication claim.")))
                      (multiple-value-bind (entry reason)
                          (task-job-enqueue-steering job "after claim")
                        (test-assert
                         (and (null entry) (eq reason ':closing))
                         "an active publication claim rejects steering"))
                      (let ((yield-result (valid-yield fixture)))
                        (test-assert
                         (and (not (tool-result-success-p yield-result))
                              (not
                               (task-completion-called-p
                                (getf fixture :completion)))
                              (not (task-job-steering-closed-p job)))
                         "an active publication claim rejects terminal yield"))
                      (test-assert
                       (and (not (agent-turn-complete-p child normal-result))
                            (not (task-job-steering-closed-p job)))
                       "an active publication claim rejects normal completion")
                      (with-lock-held (gate-lock)
                        (setf released-p t)
                        (task--condition-broadcast gate-condition))
                      (join-thread publication-thread)
                      (setf publication-thread nil)
                      (test-assert
                       (and publication-result
                            (null publication-condition)
                            (eq (job-state job) ':failed)
                            (zerop (task-job-steering-pending-count job)))
                       "publication completes after rejecting post-claim steering"))
                 (with-lock-held (gate-lock)
                   (setf released-p t)
                   (task--condition-broadcast gate-condition))
                 (when publication-thread
                   (join-thread publication-thread))
                 (unless (job-terminal-p job)
                   (setf (slot-value
                          job 'cl-jobpond::terminal-result-function)
                         original-hook))))
             (let* ((fixture (fixture "steering-terminal"))
                    (job (mark-running (getf fixture :job))))
               (task-job-enqueue-steering job "undelivered one")
               (task-job-enqueue-steering job "undelivered two")
               (task-job-take-steering job)
               (test-assert
                (= (task-job-steering-pending-count job) 2)
                "drained steering remains retained until durable acknowledgment")
               (multiple-value-bind (final-result final-report final-state)
                   (task-job--terminal-record
                    job
                    ':failed
                    (list :id (job-identifier job)
                          :name (task-job-display-name job)
                          :agent (task-job-agent-name job)
                          :assignment "Exercise terminal retention."
                          :status ':failed
                          :error "stopped")
                    nil)
                 (declare (ignore final-report))
                 (let ((artifact
                         (task-tests--read-exact-native-value
                          (uiop:read-file-string
                           (getf final-result :output-path)))))
                   (test-assert
                    (and (eq final-state ':failed)
                         (= (getf final-result :undelivered-prompt-count) 2)
                         (= (getf artifact :undelivered-prompt-count) 2)
                         (zerop (task-job-steering-pending-count job))
                         (task-job-steering-closed-p job))
                    "terminal publication records queued and in-flight prompts")))))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore)))
    nil)))
