(in-package #:autolith)

;;;; -- Default Tool Set --

(-> default-tools--register (tool-registry list) tool)
(defun default-tools--register (registry specification)
  "Create and register one default tool from SPECIFICATION."
  (destructuring-bind
      (class namespace name description parameters
       &rest initialization-arguments)
      specification
    (tool-registry-register
     registry
     (apply #'make-instance
            class
            :namespace namespace
            :name name
            :description description
            :parameters parameters
            initialization-arguments))))

(-> default-tools--required-form-schema (string) json-object)
(defun default-tools--required-form-schema (description)
  "Return a closed schema containing FORM, compilation mode, REPL, and async policy."
  (let ((properties
          (json-object
           "form" (tool-string-property description)
           "compile" (tool-boolean-property
                      "Compile before executing; defaults to false.")
           "repl" (tool-string-property
                   "The persistent REPL name; defaults to default.")
           "async" (tool-boolean-property
                    "Run as an inspectable background job; defaults to false."))))
    (tool-object-schema properties '("form"))))

(-> default-tools--lisp-repl-control-schema
    (&key (:include-image boolean))
    json-object)
(defun default-tools--lisp-repl-control-schema (&key include-image)
  "Return the shared schema for named REPL lifecycle operations."
  (let ((properties
          (json-object
           "repl" (tool-string-property
                   "The persistent REPL name; defaults to default."))))
    (when include-image
      (setf (gethash "image" properties)
            (tool-string-property
             "The pristine or saved worker image; defaults to pristine.")))
    (tool-object-schema properties nil)))

(-> default-tools--resource-operation-schema () json-object)
(defun default-tools--resource-operation-schema ()
  "Return the closed workspace, scratchpad, agenda, and memory resource.edit variants."
  (labels ((line-property (description)
             "Return one positive original-snapshot line schema."
             (json-object "type" "integer"
                          "minimum" 1
                          "description" description))

           (status-property ()
             "Return the closed agenda status schema."
             (json-object
              "type" "string"
              "enum" (vector "todo" "doing" "blocked" "done" "note")
              "description" "The agenda item's lifecycle or note status."))

           (memory-identifiers-property ()
             "Return the bounded agenda memory attachment schema."
             (json-object
              "type" "array"
              "description" "Stable memory identifiers to attach; empty detaches all."
              "maxItems" *agenda-item-memory-limit*
              "items" (tool-string-property "One stable memory identifier.")))

           (memory-tags-property ()
             "Return the bounded persistent-memory tag schema."
             (json-object
              "type" "array"
              "description" "Optional complete replacement tag list; empty clears tags."
              "maxItems" *memory-tag-count-limit*
              "items" (json-object
                       "type" "string"
                       "minLength" 1
                       "maxLength" *memory-tag-limit*)))

           (memory-scope-property ()
             "Return the closed persistent-memory replacement scope schema."
             (json-object
              "type" "string"
              "enum" (vector "global" "workspace")
              "description" "Optional complete replacement scope."))

           (operation-schema (name properties required &key any-required)
             "Return one closed resource edit operation variant."
             (setf (gethash "op" properties)
                   (json-object "type" "string"
                                "enum" (vector name)))
             (let ((schema (tool-object-schema properties (cons "op" required))))
               (when any-required
                 ;; Each anyOf variant must declare its object type: bare
                 ;; {"required": [...]} subschemas are rejected by the
                 ;; Fireworks JSON Schema validator ("could not understand the
                 ;; instance"), which fails the whole request. The validated
                 ;; instance is always an object here, so the type check does
                 ;; not change the accepted operations.
                 (setf (gethash "anyOf" schema)
                       (map 'vector
                            (lambda (field)
                              (json-object "type" "object"
                                           "required" (vector field)))
                            any-required)))
               schema)))
    (json-object
     "description"
     "One structured operation for the observed resource type."
     "oneOf"
     (vector
      (operation-schema
       "replace-lines"
       (json-object
        "start-line" (line-property "First original line to replace.")
        "end-line" (line-property "Last original line to replace.")
        "content" (tool-string-property "Replacement text, which may be empty."))
       '("start-line" "end-line" "content"))
      (operation-schema
       "delete-lines"
       (json-object
        "start-line" (line-property "First original line to delete.")
        "end-line" (line-property "Last original line to delete."))
       '("start-line" "end-line"))
      (operation-schema
       "insert-before"
       (json-object
        "line" (line-property "Visible original line before which to insert.")
        "content" (tool-string-property "Non-empty text to insert."))
       '("line" "content"))
      (operation-schema
       "insert-after"
       (json-object
        "line" (line-property "Visible original line after which to insert.")
        "content" (tool-string-property "Non-empty text to insert."))
       '("line" "content"))
      (operation-schema
       "replace-empty"
       (json-object
        "content" (tool-string-property
                   "Non-empty complete content for an observed missing resource or empty file."))
       '("content"))
      (operation-schema
       "scratchpad-delete"
       (json-object)
       nil)
      (operation-schema
       "agenda-add"
       (json-object
        "text" (tool-string-property "Complete bounded agenda item text.")
        "status" (status-property)
        "memory-ids" (memory-identifiers-property))
       '("text"))
      (operation-schema
       "agenda-update"
       (json-object
        "id" (tool-string-property "The stable agenda item identifier.")
        "text" (tool-string-property "Replacement bounded agenda item text.")
        "status" (status-property)
        "memory-ids" (memory-identifiers-property))
       '("id")
       :any-required '("text" "status" "memory-ids"))
      (operation-schema
       "agenda-remove"
       (json-object
        "id" (tool-string-property "The stable agenda item identifier."))
       '("id"))
      (operation-schema
       "memory-remember"
       (json-object
        "title" (tool-string-property "Complete persistent-memory title.")
        "content" (tool-string-property "Complete persistent-memory content.")
        "tags" (memory-tags-property))
       '("title" "content"))
      (operation-schema
       "memory-replace"
       (json-object
        "title" (tool-string-property "Complete replacement memory title.")
        "content" (tool-string-property "Complete replacement memory content.")
        "tags" (memory-tags-property)
        "scope" (memory-scope-property))
       '("title" "content"))
      (operation-schema
       "memory-forget"
       (json-object)
       nil)))))

(-> default-tools--register-workspace (tool-registry) tool-registry)
(defun default-tools--register-workspace (registry)
  "Register the default resource and image-inspection tools in REGISTRY."
  (let ((resource-registry (tool-registry-resource-registry registry)))
    (resource-registry-register
     resource-registry
     (make-instance 'workspace-file-resolver :scheme "workspace"))
    (resource-registry-register
     resource-registry
     (make-instance 'scratchpad-resolver :scheme "scratchpad"))
    (resource-registry-register
     resource-registry
     (make-instance 'agenda-resolver :scheme "agenda"))
    (resource-registry-register
     resource-registry
     (make-instance 'memory-resolver :scheme "memory"))
    (resource-registry-register
     resource-registry
     (make-instance 'inference-trace-resolver :scheme "inference"))
    (dolist
        (specification
         (list
          (list
           'resource-read-tool
           "resource" "read"
           "Read a model-addressable resource. workspace: URIs return bounded numbered file windows, sorted directory listings, or an observed missing state; scratchpad: URIs expose the current conversation's disposable files through the same bounded observations; agenda:current returns the complete current workspace agenda; memory:relevant, memory:workspace, memory:global, memory:all, and canonical memory:id/<percent-encoded-stable-id> URIs return complete memory observations. Memory collection reads optionally accept query and max-results. Direct memory:<id> remains compatible for non-reserved identifiers. inference:<trace-id> URIs return bounded read-only recursive-inference trace logs. Every read establishes a transient conversation-local revision."
           (tool-object-schema
            (json-object
             "uri" (tool-string-property
                    "The resource URI, for example workspace:src/main.lisp, workspace:src/, scratchpad:., scratchpad:program.lisp, agenda:current, memory:relevant, memory:all, or canonical memory:id/<percent-encoded-stable-id>.")
             "start-line" (tool-integer-property
                           "The first line to return, starting at 1.")
             "line-count" (tool-integer-property
                           "How many lines to return; default 400, maximum 1000.")
             "query" (tool-string-property
                      "Optional lexical query for memory collection resources.")
             "max-results" (tool-integer-property
                            "Optional memory collection result limit, capped at 50."))
            '("uri"))
           :resource-registry resource-registry)
          (list
           'resource-edit-tool
           "resource" "edit"
           "Edit a model-addressable resource at an exact observed revision. workspace: and scratchpad: files accept structured original-line operations, scratchpad: resources additionally accept scratchpad-delete, agenda:current accepts one agenda operation, memory:workspace and memory:global create with memory-remember, while canonical exact memory:id/<percent-encoded-stable-id> resources accept memory-replace or memory-forget. Workspace directories are read-only, and memory:relevant is read-only. Stale or expired revisions require a reread. Successful source-file edits may append a non-fatal delimiter warning."
           (tool-object-schema
            (json-object
             "uri" (tool-string-property
                    "The canonical resource URI returned by resource.read.")
             "base-revision" (tool-string-property
                              "The opaque revision returned by resource.read.")
             "operations" (json-object
                           "type" "array"
                           "description"
                           "Resource-specific operations. Agenda and memory resources accept exactly one; workspace: files and observed missing targets accept non-overlapping original-line operations; scratchpad: resources additionally accept one scratchpad-delete operation."
                           "minItems" 1
                           "items" (default-tools--resource-operation-schema)))
            '("uri" "base-revision" "operations"))
           :resource-registry resource-registry)
          (list
           'fs-view-image-tool
           "fs" "view-image"
           "View a local image file when visual inspection is needed. The image is returned directly to the model."
           (tool-object-schema
            (json-object
             "path" (tool-string-property
                     "The image path, absolute or workspace-relative."))
            '("path")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--register-shell (tool-registry) tool-registry)
(defun default-tools--register-shell (registry)
  "Register the workspace command tool in REGISTRY."
  (default-tools--register
   registry
   (list
    'shell-run-tool
    "shell" "run"
    "Run one external command line in the workspace, optionally as an inspectable job, and return its exit code and combined output."
    (tool-object-schema
     (json-object
      "command" (tool-string-property "The shell command line to execute.")
      "directory" (tool-string-property
                   "The working directory; defaults to the workspace.")
      "timeout-seconds" (tool-integer-property
                         "Seconds before the command is stopped; defaults to 60 with no maximum.")
      "async" (tool-boolean-property
               "Run as an inspectable background job; defaults to false."))
     '("command"))))
  registry)

(-> default-tools--register-web (tool-registry) tool-registry)
(defun default-tools--register-web (registry)
  "Register the provider-backed web search tool in REGISTRY."
  (default-tools--register
   registry
   (list
    'web-run-tool
    "web" "run"
    *web-run-description*
    (web-run-parameters)))
  registry)

(-> default-tools--register-search (tool-registry worker) tool-registry)
(defun default-tools--register-search (registry worker)
  "Register indexed workspace search tools using WORKER in REGISTRY."
  (dolist
      (specification
       (list
        (list
         'search-files-tool
         "search" "files"
         "Fuzzy-search indexed workspace file paths. Use a short one- or two-term topic, filename, or path query."
         (tool-object-schema
          (json-object
           "query" (tool-string-property
                    "A short fuzzy filename, topic, or path query.")
           "page" (tool-integer-property
                   "Zero-based result page; default 0.")
           "max-results" (tool-integer-property
                          "Results per page from 1 to 100; default 20."))
          '("query"))
         :engine worker)
        (list
         'search-glob-tool
         "search" "glob"
         "Filter indexed workspace paths with one literal glob such as **/*.lisp."
         (tool-object-schema
          (json-object
           "pattern" (tool-string-property
                      "One literal glob matched against workspace-relative paths.")
           "page" (tool-integer-property
                   "Zero-based result page; default 0.")
           "max-results" (tool-integer-property
                          "Results per page from 1 to 100; default 20."))
          '("pattern"))
         :engine worker)
        (list
         'search-content-tool
         "search" "content"
         "Search indexed workspace contents. Provide exactly one query or patterns array. Plain query matching is the default; put file or path constraints inline, or use separate constraints with literal patterns."
         (let ((schema
                 (tool-object-schema
                  (json-object
                   "query" (tool-string-property
                            "Text plus optional inline file constraints to find.")
                   "patterns" (json-object
                               "type" "array"
                               "description" "Non-empty literal alternatives searched in one pass."
                               "minItems" 1
                               "items" (json-object "type" "string"
                                                    "minLength" 1))
                   "mode" (json-object
                           "type" "string"
                           "enum" #("plain" "regex" "fuzzy")
                           "description" "Single-query matching mode; default plain.")
                   "constraints" (tool-string-property
                                  "Optional space-separated file constraints used only with patterns, such as '*.lisp src/ !tests/'.")
                   "file-offset" (tool-integer-property
                                  "Pagination cursor from next-file-offset; default 0.")
                   "max-results" (tool-integer-property
                                  "Matches returned from 1 to 100; default 20.")
                   "max-matches-per-file" (tool-integer-property
                                           "Matches retained per file from 1 to 100; default 20.")
                   "context" (tool-integer-property
                              "Lines before and after each match from 0 to 10; default 0.")
                   "time-budget-ms" (tool-integer-property
                                     "Search budget from 1 to 10000 milliseconds; default 3000."))
                  nil)))
           (setf (gethash "oneOf" schema)
                 (vector
                  (json-object "type" "object" "required" #("query"))
                  (json-object "type" "object" "required" #("patterns"))))
           schema)
         :engine worker)))
    (default-tools--register registry specification))
  registry)


(-> default-tools--register-papercut (tool-registry) tool-registry)
(defun default-tools--register-papercut (registry)
  "Register the model-facing papercut report tool in REGISTRY."
  (default-tools--register
   registry
   (list
    'papercut-report-tool
    "papercut" "report"
    "Report a concrete Autolith limitation, broken behavior, repeated friction, or blocker to the user. Use this only for a real problem that deserves prominent user attention, not routine progress or ordinary uncertainty. Include enough diagnostic context for the user to understand and reproduce the problem."
    (tool-object-schema
     (json-object
      "title" (tool-string-property
                "A concise description of the problem, at most 200 characters.")
      "content" (tool-string-property
                  "The complete concrete report for the user, at most 8000 characters."))
     '("title" "content"))))
  registry)

(-> default-tools--register-agenda (tool-registry) tool-registry)
(defun default-tools--register-agenda (registry)
  "Register workspace agenda transport in REGISTRY."
  (default-tools--register
   registry
   (list
    'agenda-transport-tool
    "agenda" "transport"
    "Enumerate or inspect workspace agendas, or copy or move one agenda to an existing workspace directory. Move rekeys an agenda after its repository changes location."
    (tool-object-schema
     (json-object
      "operation" (json-object
                   "type" "string"
                   "enum" #("workspaces" "view" "copy" "move")
                   "description" "workspaces lists known keys; view reads one; copy merges into a target while retaining the source; move merges and removes the source key.")
      "source-directory" (tool-string-property
                          "The source workspace key. Required for view, copy, and move; it may name a repository path that no longer exists.")
      "target-directory" (tool-string-property
                          "An existing destination workspace for copy or move; defaults to the current workspace."))
     '("operation"))))
  registry)

(-> default-tools--register-plan (tool-registry) tool-registry)
(defun default-tools--register-plan (registry)
  "Register workspace plan tools in REGISTRY."
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'plan-list-tool
           "plan" "list"
           "List the current workspace plan steps and optional explanation."
           empty-schema)
          (list
           'plan-update-tool
           "plan" "update"
           "Replace the current workspace plan with ordered steps, or clear it with an empty steps array. Each step has text and status pending, doing, or done. Optional explanation summarizes the plan."
           (tool-object-schema
            (json-object
             "steps"
             (json-object
              "type" "array"
              "description" "Ordered plan steps from first to last; an empty array clears the plan."
              "items"
              (json-object
               "type" "object"
               "properties"
               (json-object
                "step" (tool-string-property
                        "The work this plan point describes.")
                "status"
                (json-object
                 "type" "string"
                 "description" "pending, doing/in_progress, or done/completed."))
               "required" (json-array "step" "status")
               "additionalProperties" false))
             "explanation"
             (tool-string-property
              "Optional short explanation of the plan."))
             '("steps")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--register-lisp (tool-registry) tool-registry)
(defun default-tools--register-lisp (registry)
  "Register Lisp worker, scratchpad, and source-checking tools in REGISTRY."
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'lisp-eval-tool
           "lisp" "eval"
           "Evaluate or compile and execute one Common Lisp form in a named persistent REPL, optionally as an inspectable job."
           (default-tools--required-form-schema
            "One readable Common Lisp form."))
          (list
           'lisp-scratchpad-run-tool
           "lisp" "scratchpad-run"
           "Load one current-conversation scratchpad file into a named persistent REPL, optionally as an inspectable job."
           (tool-object-schema
            (json-object
             "path" (tool-string-property
                     "The relative scratchpad Lisp file path.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "async" (tool-boolean-property
                      "Run as an inspectable background job; defaults to false."))
            '("path")))
          (list
           'lisp-paren-check-tool
           "lisp" "paren-check"
           "Check one workspace file or directory recursively for unmatched or mismatched Common Lisp, Scheme, and Clojure delimiters."
           (tool-object-schema
            (json-object
             "path" (tool-string-property
                     "The workspace-relative or absolute source file or directory path."))
            '("path")))
          (list
           'lisp-load-system-tool
           "lisp" "load-system"
           "Load one ASDF or Quicklisp system in a named persistent REPL, optionally as an inspectable job."
           (tool-object-schema
            (json-object
             "system" (tool-string-property "The ASDF system name.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "async" (tool-boolean-property
                      "Run as an inspectable background job; defaults to false."))
            '("system")))
          (list
           'lisp-describe-tool
           "lisp" "describe"
           "Describe a readable Lisp object or symbol in a named worker or the active image."
           (tool-object-schema
            (json-object
             "designator" (tool-string-property
                           "A readable Lisp form naming the object or active symbol.")
             "target" (json-object
                       "type" "string"
                       "enum" #("worker" "self")
                       "description" "Inspect a worker by default, or the active image with self.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default."))
            '("designator")))
           (list
            'lisp-source-tool
            "lisp" "source"
            "Read matching source from a named worker or the active image."
            (tool-object-schema
             (json-object
              "name" (tool-string-property
                      "A readable definition or active symbol name.")
              "target" (json-object
                        "type" "string"
                        "enum" #("worker" "self")
                        "description" "Inspect a worker by default, or the active image with self.")
              "kind" (tool-string-property
                      "An optional SBCL definition kind, such as function, optimizer, transform, or vop.")
              "package" (tool-string-property
                         "The active-image reader package for an unqualified name; defaults to AUTOLITH.")
              "system" (tool-string-property
                        "An optional direct Autolith ASDF dependency containing the active symbol.")
              "repl" (tool-string-property
                      "The persistent REPL name; defaults to default."))
             '("name")))
          (list
           'lisp-run-tests-tool
           "lisp" "run-tests"
           "Run ASDF tests for one system in a named persistent REPL, optionally as an inspectable job."
           (tool-object-schema
            (json-object
             "system" (tool-string-property "The ASDF system name.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "async" (tool-boolean-property
                      "Run as an inspectable background job; defaults to false."))
            '("system")))
          (list
           'lisp-reset-tool
           "lisp" "reset"
           "Discard one named REPL and recreate it from pristine or a compatible saved image."
           (default-tools--lisp-repl-control-schema :include-image t))
          (list
           'lisp-start-tool
           "lisp" "start"
           "Start a named persistent REPL from pristine or a compatible saved image without silently switching an existing REPL."
           (default-tools--lisp-repl-control-schema :include-image t))
          (list
           'lisp-stop-tool
           "lisp" "stop"
           "Stop and forget one named persistent REPL."
           (default-tools--lisp-repl-control-schema))
          (list
           'lisp-repls-tool
           "lisp" "repls"
           "List named persistent REPLs, whether they are running, and their base images."
           empty-schema)
          (list
           'lisp-images-tool
           "lisp" "images"
           "List pristine and saved worker images with compatibility, parentage, and durable notes."
           empty-schema)
          (list
           'lisp-save-image-tool
           "lisp" "save-image"
           "Fork and save one named REPL as an immutable SBCL worker image, then boot-probe it and record why it exists."
           (tool-object-schema
            (json-object
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "image" (tool-string-property
                      "The new immutable image name.")
             "note" (tool-string-property
                     "What changed in this image and when to use it."))
            '("image" "note")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--register-self (tool-registry) tool-registry)
(defun default-tools--register-self (registry)
  "Register active-image inspection and mutation tools in REGISTRY."
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'self-eval-tool
           "self" "eval"
           "Evaluate one exploratory Common Lisp form in the active image."
           (tool-object-schema
            (json-object
             "form" (tool-string-property "One readable Common Lisp form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("form")))
          (list
           'self-redefine-tool
           "self" "redefine"
           "Compile and install one complete exploratory top-level definition in the active image, including Lisp-level SBCL implementation packages."
           (tool-object-schema
            (json-object
             "definition" (tool-string-property
                           "A complete defining Common Lisp form.")
             "package" (tool-string-property
                        "The active package in which to read and install the definition; defaults to AUTOLITH.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("definition")))
          (list
           'self-set-tool
           "self" "set"
           "Set one active global binding to the value of a Common Lisp form."
           (tool-object-schema
            (json-object
             "symbol" (tool-string-property "The active global symbol name.")
             "value" (tool-string-property "A Common Lisp value form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("symbol" "value")))
          (list
           'self-persist-definition-tool
           "self" "persist-definition"
           "Compile, install, check, and persist one complete definition in a private image commit backed by Autolith's private mutation-history Git repository. The tracked source repository is never modified."
           (tool-object-schema
            (json-object
             "definition" (tool-string-property
                           "A complete defining Common Lisp form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("definition")))
          (list
           'self-status-tool
           "self" "status"
           "Summarize running and selected private image state, effective pending mutations, and retained generations."
           empty-schema)
          (list
           'self-discard-tool
           "self" "discard"
           "Restore and discard the newest effective exploratory mutation, or the effective mutation with a specified identifier."
           (tool-object-schema
            (json-object
             "mutation" (tool-string-property
                         "An effective mutation identifier from self.diff; defaults to the newest effective mutation."))
            nil))
          (list
           'self-exercise-tool
           "self" "exercise"
           "Evaluate and journal one focused assertion-style Common Lisp form against an effective pending mutation. A signaled condition fails the exercise; this does not replace self.commit's full checks."
           (tool-object-schema
            (json-object
             "form" (tool-string-property
                     "One Common Lisp exercise form; use ASSERT or ERROR to signal failure.")
             "mutation" (tool-string-property
                         "An effective mutation identifier from self.diff; defaults to the newest effective mutation."))
            '("form")))
          (list
           'self-diff-tool
           "self" "diff"
           "Show the effective self.redefine and self.set changes not yet persisted by self.commit, collapsing repeated edits to each target."
           empty-schema)
          (list
           'self-commit-tool
           "self" "commit"
           "Check and persist all pending self.redefine and self.set mutations as an immutable private image commit and complete Lisp replay script, then retain the snapshot in Autolith's private mutation-history Git repository. This never changes a workspace repository."
           (tool-object-schema
            (json-object
             "title" (tool-string-property
                      "A single private image-commit title under 72 characters."))
            '("title")))
          (list
           'self-checkpoint-tool
           "self" "checkpoint"
           "Validate and asynchronously save the active image as a generation."
           empty-schema)
          (list
           'self-generations-tool
           "self" "generations"
           "List retained generations and whether this SBCL can boot them."
           empty-schema)
          (list
           'self-rollback-tool
           "self" "rollback"
           "Select a compatible retained generation and request immediate rollback."
           (tool-object-schema
            (json-object
             "generation" (tool-string-property
                           "The retained generation identifier."))
            '("generation")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--remove-mutable-self-tools (tool-registry) tool-registry)
(defun default-tools--remove-mutable-self-tools (registry)
  "Remove every mutable active-image tool from REGISTRY."
  (tool-registry-delete-if registry
                           (lambda (tool)
                             (typep tool 'mutable-self-tool))))

(-> make-default-tool-registry (&key (:immutable-p boolean)) tool-registry)
(defun make-default-tool-registry (&key immutable-p)
  "Create Autolith's tool registry, omitting mutable self tools when requested."
  (let ((registry (make-instance 'tool-registry))
        (search-worker (search-worker-create)))
    (default-tools--register-workspace registry)
    (default-tools--register-web registry)
    (default-tools--register-search registry search-worker)
    (default-tools--register-shell registry)
    (default-tools--register-papercut registry)
    (default-tools--register-agenda registry)
    (default-tools--register-plan registry)
    (default-tools--register-lisp registry)
    (default-tools--register-self registry)
    (rlm-register-tools registry)
    (skill-augment-tool-registry registry)
    (when immutable-p
      (default-tools--remove-mutable-self-tools registry))
    registry))
