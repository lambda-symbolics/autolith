(in-package #:autolith)

;;;; -- Default Tool Set --

(-> default-tools--register
    (tool-registry list &key (:correction-metadata-p boolean)) tool)
(defun default-tools--register (registry specification &key (correction-metadata-p t))
  "Create and register one default tool with optional correction metadata."
  (destructuring-bind
      (class namespace name description parameters
       &rest initialization-arguments)
      specification
    (when correction-metadata-p
      (setf (gethash "corrects_previous" (json-get parameters "properties"))
            (tool-boolean-property "Optional: true only when this call explicitly corrects the immediately preceding tool call.")))
    (tool-registry-register
     registry
     (apply #'make-instance
            class
            :namespace namespace
            :name name
            :description description
            :parameters parameters
             :correction-metadata-p correction-metadata-p
            initialization-arguments))))

(-> default-tools--worker-host-tools-property () json-object)
(defun default-tools--worker-host-tools-property ()
  "Return the explicit bounded worker callback capability schema."
  (json-object
   "type" "array"
   "items" (json-object "type" "string" "minLength" 1)
   "maxItems" 64
   "description" "Optional exact host tool names available to this request. Omit for plain worker execution; ordinary originating authority still applies."))

(-> default-tools--required-form-schema (string) json-object)
(defun default-tools--required-form-schema (description)
  "Return a closed schema containing FORMS, compilation mode, REPL, and async policy."
  (let ((properties
          (json-object
           "forms" (tool-forms-property description)
           "compile" (tool-boolean-property
                      "Compile before executing; defaults to false.")
           "repl" (tool-string-property
                   "The persistent REPL name; defaults to default.")
            "host-tools" (default-tools--worker-host-tools-property)
           "completion-policy" (tool-completion-policy-property)
           "async" (tool-boolean-property
                    "Run as a background job and return at once. In the primary session its completion wakes you, so do not poll it. Defaults to false."))))
    (tool-object-schema properties '("forms"))))

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

(-> default-tools--resource-operation-schema (&key (:knowledge-p boolean)) json-object)
(defun default-tools--resource-operation-schema (&key (knowledge-p t))
  "Return the closed workspace, scratchpad, agenda, memory, and papercut resource.edit variants.

KNOWLEDGE-P adds the memory:facts operations defining predicates, asserting and
retracting facts, and proposing rules."
  (labels ((line-property (description)
             "Return one positive original-snapshot line schema."
             (let ((schema (tool-integer-property description)))
               (setf (gethash "minimum" schema) 1)
               schema))

           (anchor-property (description)
             "Return one optional short hashline anchor schema."
             (let ((schema (tool-string-property description)))
               (setf (gethash "pattern" schema) "^[0-9A-Fa-f]{4}$")
               schema))

           (status-property ()
             "Return the closed agenda status schema."
             (cl-llm-provider-api:provider-enum-schema
              '("todo" "doing" "blocked" "done" "note")
              "The agenda item's lifecycle or note status."))

           (memory-identifiers-property ()
             "Return the bounded agenda memory attachment schema."
             (let ((schema
                     (cl-llm-provider-api:provider-array-schema
                      (tool-string-property "One stable memory identifier.")
                      "Stable memory identifiers to attach; empty detaches all.")))
               (setf (gethash "maxItems" schema) *agenda-item-memory-limit*)
               schema))

           (memory-tags-property ()
             "Return the bounded persistent-memory tag schema."
             (let ((schema
                     (cl-llm-provider-api:provider-array-schema
                      (json-object "type" "string"
                                   "minLength" 1
                                   "maxLength" *memory-tag-limit*)
                      "Optional complete replacement tag list; empty clears tags.")))
               (setf (gethash "maxItems" schema) *memory-tag-count-limit*)
               schema))

           (memory-scope-property ()
             "Return the closed persistent-memory replacement scope schema."
             (cl-llm-provider-api:provider-enum-schema
              '("global" "workspace")
              "Optional complete replacement scope."))

           (memory-sources-property ()
             "Return the bounded persistent-memory evidence schema."
             (let ((schema
                     (cl-llm-provider-api:provider-array-schema
                      (tool-object-schema
                       (json-object
                        "file" (tool-string-property "A cited file path.")
                        "url" (tool-string-property "A cited http or https address.")
                        "user" (tool-string-property
                                "The user's exact words in this conversation.")
                        "quote" (tool-string-property
                                 "The exact supporting passage of the file or page; a file quote must occur in the file now."))
                       nil)
                      "Optional evidence the memory was written from; each object names one of file, url, or user. A memory whose quoted file text changes is reported stale. Replacing without sources keeps the recorded ones, so a stale memory stays stale until current sources are supplied; empty removes them.")))
               (setf (gethash "maxItems" schema) *memory-source-count-limit*)
               schema))

           (memory-premises-property ()
             "Return the bounded persistent-memory premise schema."
             (let ((schema
                     (cl-llm-provider-api:provider-array-schema
                      (tool-string-property "One premise memory identifier.")
                      "Optional memories this one is derived from; it is reported stale when one changes. Replacing without premises keeps the recorded premise versions; supply them again only after checking the memory still follows from them.")))
               (setf (gethash "maxItems" schema) *memory-premise-count-limit*)
               schema))

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
     (concatenate
      'vector
      (when knowledge-p
        (list
      (operation-schema
       "predicate-define"
       (json-object
        "predicate" (tool-string-property "Lowercase dotted name such as event.start.")
        "arguments" (cl-llm-provider-api:provider-array-schema
                     (tool-object-schema
                      (json-object
                       "name" (tool-string-property "Lowercase argument name.")
                       "type" (cl-llm-provider-api:provider-enum-schema
                               *memory-knowledge-type-names* "The argument's value type.")
                       "unit" (tool-string-property
                               "For a quantity, a unit whose dimension values must have."))
                      '("name" "type"))
                     "Typed arguments in order.")
        "key" (cl-llm-provider-api:provider-array-schema
               (tool-string-property "An argument name.")
               "Arguments identifying the subject; facts agreeing on them and differing elsewhere conflict.")
        "template" (tool-string-property "Sentence with {argument} placeholders.")
        "description" (tool-string-property "What the predicate means.")
        "scope" (memory-scope-property))
       '("predicate" "arguments"))
      (operation-schema
       "fact-assert"
       (json-object
        "predicate" (tool-string-property "A defined predicate.")
        "arguments" (json-object "type" "array"
                                 "items" (json-object "description" "One argument value.")
                                 "description" "Values in argument order: strings, numbers, ISO instants and dates, durations such as PT30M, quantities as {magnitude, unit}.")
        "negated" (tool-boolean-property "Assert that the fact does not hold.")
        "sources" (memory-sources-property)
        "basis" (cl-llm-provider-api:provider-enum-schema
                 '("observation" "user-assertion" "assumption" "model-proposal")
                 "Why it is believed. The sources decide it: a file is an observation, the user's words a user assertion, a page an observation only once retrieved with that text in this conversation, and no source a model proposal. Give assumption or model-proposal to claim less.")
        "valid-from" (tool-string-property "ISO instant from which the fact holds.")
        "valid-until" (tool-string-property "ISO instant after which it no longer holds.")
        "scope" (memory-scope-property))
       '("predicate" "arguments"))
      (operation-schema
       "method-define"
       (json-object
        "id" (tool-string-property "Lowercase method name; defining it again replaces it.")
        "task" (json-object "type" "array"
                            "items" (json-object "description" "A task name, then variables or values.")
                            "description" "The task this method does, such as [\"prepare\", \"?event\"].")
        "when" (json-object "type" "array"
                            "items" (json-object "description" "One fact pattern.")
                            "description" "Fact patterns that must hold for this method to apply.")
        "steps" (json-object "type" "array"
                             "items" (json-object "description" "{\"task\": [NAME, ARGS...]} or {\"step\": TEXT, \"requires\": [...], \"verify\": PATTERN}.")
                             "description" "Ordered subtasks and plain steps; {?var} in step text is filled from facts.")
        "description" (tool-string-property "What the method achieves.")
        "scope" (memory-scope-property))
       '("id" "task" "steps"))
      (operation-schema
       "contract-define"
       (json-object
        "id" (tool-string-property "Lowercase contract name; defining it again replaces it.")
        "tool" (tool-string-property "The tool the contract describes, such as shell.run or an MCP tool.")
        "when" (json-object "type" "object"
                            "description" "Argument names mapped to required values or to variables they bind.")
        "requires" (json-object "type" "array"
                                "items" (json-object "description" "One fact pattern.")
                                "description" "Fact patterns that must hold before the call; otherwise it is refused.")
        "effects" (json-object "type" "array"
                               "items" (json-object "description" "One fact pattern.")
                               "description" "Facts the call is expected to cause, recorded as predictions.")
        "observes" (json-object "type" "array"
                                "items" (json-object "description" "{\"predicate\": NAME, \"arguments\": [VALUE, ?var, $arg.NAME, or $result.PATH]}.")
                                "description" "Facts read from the call's arguments and JSON result after it succeeds.")
        "accepts" (json-object "type" "array"
                               "items" (cl-llm-provider-api:provider-enum-schema
                                        '("observation" "user-assertion" "assumption" "model-proposal")
                                        "A basis a requirement may rest on.")
                               "description" "Bases requirements may rest on; default observation and user-assertion. Add model-proposal to let earlier calls' predicted effects count.")
        "description" (tool-string-property "What the contract states.")
        "scope" (memory-scope-property))
       '("id" "tool"))
      (operation-schema
       "fact-retract"
       (json-object
        "id" (tool-string-property "A claim or support identifier.")
        "reason" (tool-string-property "Why it no longer holds."))
       '("id"))
      (operation-schema
       "rule-propose"
       (json-object
        "id" (tool-string-property "Lowercase rule name; proposing it again makes a new version.")
        "premises" (json-object "type" "array"
                                "items" (json-object "description" "One premise pattern.")
                                "description" "Patterns such as [\"event.start\", \"?event\", \"?start\"], or [\"not\", PATTERN] matching an explicitly asserted negation.")
        "guards" (json-object "type" "array"
                              "items" (json-object "description" "One comparison.")
                              "description" "Comparisons such as [\"<\", \"?a\", \"?b\"].")
        "bindings" (json-object "type" "array"
                                "items" (json-object "description" "One variable and its expression.")
                                "description" "New variables such as [\"?leave\", [\"-\", \"?start\", \"?travel\"]], computed with + - * / min max abs date start-of-day weekday days hours minutes in-unit.")
        "conclusion" (json-object "type" "array"
                                  "items" (json-object "description" "A predicate name, variable, or value.")
                                  "description" "The pattern the rule concludes, using bound variables.")
        "description" (tool-string-property "What the rule does.")
        "scope" (memory-scope-property))
       '("id" "premises" "conclusion"))))
      (vector
       (operation-schema
        "replace-lines"
        (json-object
         "start-line" (line-property "First original line to replace.")
         "start-anchor" (anchor-property
                         "Optional anchor printed beside the first line; corrects a small transcription offset.")
         "end-line" (line-property "Last original line to replace.")
         "end-anchor" (anchor-property
                       "Optional anchor printed beside the last line; corrects a small transcription offset.")
         "content" (tool-string-property "Replacement text, which may be empty."))
        '("start-line" "end-line" "content"))
       (operation-schema
        "delete-lines"
        (json-object
         "start-line" (line-property "First original line to delete.")
         "start-anchor" (anchor-property
                         "Optional anchor printed beside the first line; corrects a small transcription offset.")
         "end-line" (line-property "Last original line to delete.")
         "end-anchor" (anchor-property
                       "Optional anchor printed beside the last line; corrects a small transcription offset."))
        '("start-line" "end-line"))
       (operation-schema
        "insert-before"
        (json-object
         "line" (line-property "Visible original line before which to insert.")
         "anchor" (anchor-property
                   "Optional anchor printed beside the line; corrects a small transcription offset.")
         "content" (tool-string-property "Non-empty text to insert."))
        '("line" "content"))
       (operation-schema
        "insert-after"
        (json-object
         "line" (line-property "Visible original line after which to insert.")
         "anchor" (anchor-property
                   "Optional anchor printed beside the line; corrects a small transcription offset.")
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
        "text" (tool-string-property
                (format nil "Complete agenda item text, 1 to ~D characters."
                        *agenda-item-text-limit*))
        "status" (status-property)
        "memory-ids" (memory-identifiers-property))
       '("text"))
      (operation-schema
       "agenda-update"
       (json-object
        "id" (tool-string-property "The stable agenda item identifier.")
        "text" (tool-string-property
                (format nil "Replacement agenda item text, 1 to ~D characters."
                        *agenda-item-text-limit*))
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
        "tags" (memory-tags-property)
        "sources" (memory-sources-property)
        "premises" (memory-premises-property))
       '("title" "content"))
      (operation-schema
       "memory-replace"
       (json-object
        "title" (tool-string-property "Complete replacement memory title.")
        "content" (tool-string-property "Complete replacement memory content.")
        "tags" (memory-tags-property)
        "scope" (memory-scope-property)
        "sources" (memory-sources-property)
        "premises" (memory-premises-property))
       '("title" "content"))
      (operation-schema
       "memory-forget"
       (json-object)
       nil)
      (operation-schema
       "papercut-report"
       (json-object
        "issue-kind" (cl-llm-provider-api:provider-enum-schema
                      '("broken_tool" "misleading_success" "authorization" "performance" "other")
                      "Explicit issue category; omit when unknown.")
        "tool" (tool-string-property "Affected builtin tool name; omit when unknown.")
        "title" (tool-string-property "Complete bounded papercut title.")
        "content" (tool-string-property "Complete bounded private local papercut report.")
        "diagnostic" (default-tools--diagnostic-property))
       '("title" "content"))
      (operation-schema
       "papercut-assess"
       (json-object
        "verdict" (json-object
                   "type" "string"
                   "description" "Latest effectiveness verdict."
                   "enum" (vector "improved" "worse" "unchanged" "too-early"))
        "note" (tool-string-property
                "Complete bounded private local assessment note.")
        "diagnostic" (default-tools--diagnostic-property))
       '("verdict" "note"))
      (operation-schema
       "papercut-close"
       (json-object
        "resolution" (tool-string-property
                      "Complete bounded private local closure resolution.")
        "diagnostic" (default-tools--diagnostic-property))
       '("resolution")))))))

(-> default-tools--register-workspace (tool-registry &key (:knowledge-p boolean)) tool-registry)
(defun default-tools--register-workspace (registry &key (knowledge-p t))
  "Register the default resource and image-inspection tools in REGISTRY.

KNOWLEDGE-P offers structured knowledge through memory: resources."
  (tool-registry-describe-namespace
   registry "resource"
   "Revision-gated observation and structured editing of model-addressable resources.")
  (tool-registry-describe-namespace
   registry "fs"
   "Workspace file reading, listing, complete writes, and compatibility editing.")
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
     (make-instance 'memory-resolver :scheme "memory" :knowledge-p knowledge-p))
    (resource-registry-register
     resource-registry
     (make-instance 'papercut-resolver :scheme "papercut"))
    (resource-registry-register
     resource-registry
     (make-instance 'inference-trace-resolver :scheme "inference"))
    (resource-registry-register
     resource-registry
     (make-instance 'context-object-resolver :scheme "context"))
    (resource-registry-register
     resource-registry
     (make-instance 'conversation-resolver :scheme "conversation"))
    (resource-registry-register
     resource-registry
     (make-instance 'shell-log-resolver :scheme "shell-log"))
    (dolist
        (specification
         (list
          (list
           'resource-read-tool
           "resource" "read"
           "Read a model-addressable resource."
           (tool-object-schema
            (json-object
             "uri" (tool-string-property
                    "A resource URI, SCHEME:IDENTIFIER with a listed scheme, or an absolute native workspace file path. Relative paths require workspace:.")
             "start-line" (tool-integer-property
                           "The first line to return, starting at 1. Only line-windowed resources accept start-line and line-count; the others reject them.")
             "line-count" (tool-integer-property
                           "How many lines to return; default 400, maximum 1000. Accepted only where start-line is.")
             "byte-offset" (tool-integer-property
                            "Zero-based byte offset for a shell-log: range or search page; defaults to 0.")
             "byte-count" (tool-integer-property
                           "Maximum raw bytes in a shell-log: range, capped by the log resource adapter.")
             "start-sequence" (tool-integer-property
                               "The first durable record sequence a conversation: window returns; without it the window ends at the newest record.")
             "record-count" (tool-integer-property
                             "How many records a conversation: window returns; default 20, maximum 200.")
             "query" (tool-string-property
                       "Optional lexical memory/conversation query, or literal shell-log: text search.")
             "max-results" (tool-integer-property
                            "Optional memory, conversation, or shell-log: search result limit, capped at 50."))
            '("uri"))
           :resource-registry resource-registry)
          (list
           'resource-edit-tool
           "resource" "edit"
           "Edit a model-addressable resource at an exact revision observed by resource.read. Stale or expired revisions require a reread."
           (tool-object-schema
            (json-object
             "uri" (tool-string-property
                    "The canonical resource URI returned by resource.read.")
             "base-revision" (tool-string-property
                              "The opaque revision returned by resource.read.")
             "operations" (json-object
                           "type" "array"
                           "description"
                           "Resource-specific operations. Agenda, memory, and papercut resources accept exactly one; line-edited text and observed missing files accept non-overlapping original-line operations; scratchpad: resources additionally accept one scratchpad-delete operation."
                           "minItems" 1
                           "items" (default-tools--resource-operation-schema
                                    :knowledge-p knowledge-p)))
            '("uri" "base-revision" "operations"))
           :resource-registry resource-registry)
          (list
           'fs-view-image-tool
           "fs" "view-image"
           "View one local PNG, JPEG, GIF, or WebP image when visual inspection is needed; the image is returned directly to the model. It reads only images: read source, text, HTML, PDF, and every other file through resource.read with a workspace: URI instead. The path must lie inside the workspace or the Autolith source root, so save screenshots and renders into the workspace before viewing them."
           (tool-object-schema
            (json-object
             "path" (tool-string-property
                     "The image path, absolute or workspace-relative, inside the workspace or source root."))
            '("path")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--register-shell (tool-registry) tool-registry)
(defun default-tools--register-shell (registry)
  "Register the workspace command tool in REGISTRY."
  (tool-registry-describe-namespace
   registry "shell"
   "External commands run in the workspace.")
  (default-tools--register
   registry
   (list
    'shell-run-tool
    "shell" "run"
     "Run one authorized external command, synchronously or as an inspectable job. Retain private raw output and return exit/status metadata, a bounded head/tail preview, and shell-log: references for later ranges or search. Use PowerShell syntax on Windows and POSIX shell syntax elsewhere."
    (tool-object-schema
     (json-object
      "command" (tool-string-property "The shell command line to execute.")
      "description"
      (tool-string-property
       "An optional short description used to label the live command row.")
      "directory" (tool-string-property
                   "The working directory; defaults to the workspace.")
      "timeout-seconds" (tool-integer-property
                         "Seconds before the command is stopped; defaults to 60 with no maximum.")
       "separate-output" (tool-boolean-property
                          "Capture stdout/stderr separately; defaults to false (merged output). Separate streams have no reconstructed interleaving.")
      "completion-policy" (tool-completion-policy-property)
      "async" (tool-boolean-property
               "Run as a background job and return at once. In the primary session its completion wakes you, so do not poll it. Defaults to false."))
     '("command"))))
  registry)

(-> default-tools--register-web (tool-registry) tool-registry)
(defun default-tools--register-web (registry)
  "Register the web search and web page retrieval tools in REGISTRY."
  (default-tools--register
   registry
   (list
    'web-run-tool
    "web" "run"
    *web-run-description*
     (web-run-parameters))
    ;; The subscription backend validates this provider-reserved schema exactly.
    :correction-metadata-p nil)
  (default-tools--register
   registry
   (list
    'web-gist-tool
    "web_extra" "gist"
    "Retrieve one HTML, Markdown, or plain-text page over HTTP or HTTPS, following redirects. HTML is converted to Markdown; Markdown and plain text are returned as text. Supply a known URL from the user or search results. JSON responses, such as REST API replies, are refused, so fetch those with shell.run instead."
    (web-gist-parameters)))
  registry)

(-> default-tools--register-search (tool-registry worker) tool-registry)
(defun default-tools--register-search (registry worker)
  "Register indexed workspace search tools using WORKER in REGISTRY."
  (tool-registry-describe-namespace
   registry "search"
   "Fast indexed workspace path and content discovery through fff.")
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
           "Search indexed workspace contents. Provide exactly one of query or patterns. Path filters go in constraints, for example '*.lisp src/ docs/ !tests/'."
         (let ((schema
                 (tool-object-schema
                  (json-object
                   "query" (tool-string-property
                              "Search text. One path filter may be written inline, for example '*.lisp symbol'; put several in constraints. A Lisp special variable name such as *limit* is searched as text.")
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
                                    "Optional space-separated path filters. Directories and file paths are alternatives, as are *.ext filters, and !filters exclude, so '*.lisp src/ tests/ !tests/fixtures/' searches Lisp files under src/ or tests/ outside tests/fixtures/. A file path must name an existing file. Valid with query or patterns.")
                   "file-offset" (tool-integer-property
                                  "Pagination cursor from next-file-offset; default 0.")
                   "max-results" (tool-integer-property
                                  "Matches per page from 1 to 100; default 20. A page keeps one file's matches together, so it can exceed this by up to max-matches-per-file.")
                   "max-matches-per-file" (tool-integer-property
                                           "Matches retained per file from 1 to 100; default 20. The result names files that had more.")
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
    "Report a concrete Autolith limitation, broken behavior, repeated friction, or blocker to the user. Use this only for a real problem that deserves prominent user attention, not routine progress or ordinary uncertainty. Include enough diagnostic context for the user to understand and reproduce the problem. Report each problem once: Autolith compares the report with the active papercuts of this workspace and answers with the existing report instead of recording a repeat, and the acknowledgement stays in the conversation."
    (tool-object-schema
     (json-object
      "title" (tool-string-property
                "A concise description of the problem, at most 200 characters.")
      "content" (tool-string-property
                  "The complete private local report for the user, at most 8000 characters.")
      "diagnostic" (default-tools--diagnostic-property)
      "issue-kind" (cl-llm-provider-api:provider-enum-schema
                    '("broken_tool" "misleading_success" "authorization" "performance" "other")
                    "Explicit issue category; omit when unknown.")
      "tool" (tool-string-property "Affected builtin tool name; omit when unknown."))
     '("title" "content"))))
  registry)

(-> default-tools--register-agenda (tool-registry) tool-registry)
(defun default-tools--register-agenda (registry)
  "Register workspace agenda transport in REGISTRY."
  (tool-registry-describe-namespace
   registry "agenda"
   "Short persistent tasks and notes keyed by workspace directory.")
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

(-> default-tools--register-procedure (tool-registry) tool-registry)
(defun default-tools--register-procedure (registry)
  "Register the tool running learned procedures in REGISTRY."
  (tool-registry-describe-namespace
   registry "procedure"
   "Tool-call sequences Autolith learned from recurring work, run in one step.")
  (default-tools--register
   registry
   (list 'procedure-run-tool
         "procedure" "run"
         "Make a learned procedure's tool calls in one step, each with its usual authorization, and report every call and result. Use it when Autolith says a procedure fits the request, or one listed by memory:procedures does. A procedure never admitted asks for admission first. If a call fails the run stops and reports where; continue with ordinary calls."
         (tool-object-schema
          (json-object
           "id" (tool-string-property "The procedure's name.")
           "parameters" (json-object "type" "object"
                                     "description" "The procedure's parameters by name, such as {\"quarter\": \"q3\"}."))
          '("id"))))
  registry)

(-> default-tools--register-diagnosis (tool-registry) tool-registry)
(defun default-tools--register-diagnosis (registry)
  "Register the tool testing hypotheses with probes in REGISTRY."
  (tool-registry-describe-namespace
   registry "diagnosis"
   "Hypothesis tests that run the probe calls telling causes apart.")
  (let ((pattern-array
          (json-object "type" "array"
                       "items" (json-object "type" "array"
                                            "items" (json-object
                                                     "description" "A predicate, then values or parameters such as ?service.")
                                            "description" "One fact pattern, or [\"not\", PATTERN]."))))
    (default-tools--register
     registry
     (list 'diagnosis-run-tool
           "diagnosis" "run"
           "Find which of up to 4 hypotheses explains a failure before repairing it. Each hypothesis predicts facts; each probe is a tool call whose contract observes facts from its result (define contracts with contract-define on memory:facts). Known facts are checked first, then the probe telling apart the most remaining hypotheses per cost runs, at most 3, with no model request between them. A hypothesis is ruled out only by a fact contradicting its prediction. Reports the remaining hypothesis and its repair, or what is still open, and which facts decided."
           (tool-object-schema
            (json-object
             "diagnosis" (tool-string-property "The name of a saved diagnosis to run.")
             "bindings" (json-object "type" "object"
                                     "description" "Values for the diagnosis's parameters, such as {\"service\": \"billing\"}.")
             "hypotheses" (json-object
                           "type" "array"
                           "items" (tool-object-schema
                                    (json-object
                                     "id" (tool-string-property "Lowercase hypothesis name.")
                                     "predicts" pattern-array
                                     "repair" (tool-string-property "What to do if it is the cause.")
                                     "description" (tool-string-property "What the hypothesis says."))
                                    '("id" "predicts"))
                           "description" "The hypotheses, when no saved diagnosis is named.")
             "probes" (json-object
                       "type" "array"
                       "items" (tool-object-schema
                                (json-object
                                 "tool" (tool-string-property "The probe's tool, such as service.ping.")
                                 "arguments" (json-object "type" "object"
                                                          "description" "Its arguments; ?name takes a parameter.")
                                 "cost" (json-object "type" "number"
                                                     "description" "Its relative cost; default 1."))
                                '("tool"))
                       "description" "The tool calls that can observe predicted facts.")
             "parameters" (json-object "type" "array"
                                       "items" (tool-string-property "A parameter name.")
                                       "description" "Parameters the hypotheses and probes use, for save_as.")
             "save_as" (tool-string-property "Save the given hypotheses and probes under this name to run again."))
            nil))))
  registry)

(-> default-tools--register-plan (tool-registry &key (:knowledge-p boolean)) tool-registry)
(defun default-tools--register-plan (registry &key (knowledge-p t))
  "Register workspace plan tools in REGISTRY.

KNOWLEDGE-P lets steps name the facts they require and the fact that verifies
them."
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
           (format nil "Replace the current workspace plan with ordered steps, or clear it with an empty steps array. Each step has text and status pending, doing, or done. Optional explanation summarizes the plan.~:[~; A step may name the facts it requires: their variables fill {?var} in its text from current facts, and the plan reports a step blocked while nothing satisfies them or done on facts that later stopped holding.~]"
                   knowledge-p)
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
               (let ((properties
                       (json-object
                        "step" (tool-string-property
                                "The work this plan point describes.")
                        "status"
                        (json-object
                         "type" "string"
                         "description" "pending, doing/in_progress, or done/completed."))))
                 (when knowledge-p
                   (setf (gethash "id" properties)
                         (tool-string-property "A stable step name; keeps its record across updates.")
                         (gethash "requires" properties)
                         (json-object "type" "array"
                                      "items" (json-object "description" "One fact pattern.")
                                      "description" "Fact patterns such as [\"event.departure\", \"review\", \"?leave\"] the step needs.")
                         (gethash "verify" properties)
                         (json-object "type" "array"
                                      "items" (json-object "description" "A predicate, variable, or value.")
                                      "description" "A fact pattern whose observation confirms the step was done.")))
                 properties)
               "required" (json-array "step" "status")
               "additionalProperties" (json-false)))
             "explanation"
             (tool-string-property
              "Optional short explanation of the plan."))
             '("steps")))))
      (default-tools--register registry specification)))
  (when knowledge-p
    (default-tools--register
     registry
     (list 'plan-expand-tool
           "plan" "expand"
           "Replace the current workspace plan with a task expanded locally by its methods (see memory:methods), checked against current facts. Reports a task no method describes, methods that do not apply, or a search that ran out of budget, without changing the plan."
           (tool-object-schema
            (json-object
             "task" (json-object "type" "array"
                                 "items" (json-object "description" "The task name, then its argument values.")
                                 "description" "The task to plan, such as [\"prepare\", \"review\"]."))
            '("task")))))
  registry)

(-> default-tools--register-lisp (tool-registry) tool-registry)
(defun default-tools--register-lisp (registry)
  "Register Lisp worker, scratchpad, and source-checking tools in REGISTRY."
  (tool-registry-describe-namespace
   registry "lisp"
   "Named Common Lisp workers, scratchpads, and Lisp-family source checks.")
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'lisp-eval-tool
           "lisp" "eval"
           "Evaluate, or compile and execute, Common Lisp forms in order in a named persistent REPL, optionally as an inspectable job. Each form is read only after the one before it ran, so a later form may use a package an earlier form loaded. The result shows the last form's values; a failure stops the sequence and names the failing form."
           (default-tools--required-form-schema
            "The forms to run in order, each string holding exactly one form."))
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
             "host-tools" (default-tools--worker-host-tools-property)
             "completion-policy" (tool-completion-policy-property)
             "async" (tool-boolean-property
                      "Run as a background job and return at once. In the primary session its completion wakes you, so do not poll it. Defaults to false."))
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
             "asd" (tool-string-property
                     "An optional path to the exact ASD file defining the system.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "completion-policy" (tool-completion-policy-property)
             "async" (tool-boolean-property
                      "Run as a background job and return at once. In the primary session its completion wakes you, so do not poll it. Defaults to false."))
            '("system")))
          (list
           'lisp-describe-tool
           "lisp" "describe"
           "Describe a readable Lisp object or symbol in a named worker or the active image."
           (tool-object-schema
            (json-object
             "designator" (tool-string-property
                           "A readable Lisp form naming the object or active symbol; a bare or quoted symbol name works.")
             "target" (json-object
                       "type" "string"
                       "enum" #("worker" "self")
                       "description" "Inspect a worker by default, or the active image with self.")
             "package" (tool-string-property
                        "The active-image reader package for an unqualified name; defaults to AUTOLITH.")
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
           'lisp-apropos-tool
           "lisp" "apropos"
           "Find defined names in the active image before asking lisp.describe or lisp.source for an exact one. Lists every function, macro, generic function, class, condition, variable, or type whose name contains all the query terms, with its kinds, lambda list, first documentation line, and tracked source file. Never guess a definition name: search for it here or with search.content first."
           (tool-object-schema
            (json-object
             "query" (tool-string-property
                      "One or more whitespace-separated name fragments that must all occur in the symbol name, matched without regard to case, for example \"retry provider\".")
             "package" (tool-string-property
                        "The package to search; defaults to AUTOLITH. Dependency packages such as CL-LLM-PROVIDER-API are searched by name.")
             "kind" (json-object
                     "type" "string"
                     "enum" #("function" "macro" "generic-function" "class" "condition" "variable" "type")
                     "description" "Optional definition kind that every listed name must carry.")
             "limit" (tool-integer-property
                      "How many matches to list; default 40, maximum 200."))
            '("query")))
          (list
           'lisp-run-tests-tool
           "lisp" "run-tests"
           "Run ASDF tests for one system in a named persistent REPL, optionally as an inspectable job."
           (tool-object-schema
            (json-object
             "system" (tool-string-property "The ASDF system name.")
             "asd" (tool-string-property
                     "An optional path to the exact ASD file defining the system.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "completion-policy" (tool-completion-policy-property)
             "async" (tool-boolean-property
                      "Run as a background job and return at once. In the primary session its completion wakes you, so do not poll it. Defaults to false."))
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

(-> default-tools--diagnostic-property () hash-table)
(defun default-tools--diagnostic-property ()
  "Describe an explicit explanation intended for optional redacted telemetry."
  (json-object "type" "string" "minLength" 1 "maxLength" *telemetry-diagnostic-limit*
               "description" "Diagnostic explanation for optional redacted export, up to 8,000 characters. Explain observed behavior, evidence and suspected cause; for a repair explain the change and why, and for a check describe what it verifies. Separate facts from hypotheses and state uncertainty. Exclude private details, raw tool input/output, prompts, patches and stack traces."))

(-> default-tools--self-repair-schema (list) list)
(defun default-tools--self-repair-schema (specification)
  "Add optional explicit repair metadata to self mutation and exercise schemas."
  (let* ((class (first specification))
         (properties (json-get (fifth specification) "properties")))
    (when (subtypep class 'mutable-self-tool)
      (setf (gethash "diagnostic" properties) (default-tools--diagnostic-property)))
    (when (member class '(self-eval-tool self-redefine-tool self-set-tool
                         self-persist-definition-tool self-exercise-tool))
      (setf (gethash "incident" properties)
            (tool-string-property "The exact active papercut identifier for this repair; omit for a standalone mutation.")
            (gethash "repair-kind" properties)
            (json-object "type" "string"
                         "enum" #("redefine_function" "change_setting" "change_tool_strategy" "other")
                         "description" "The public repair category; checks inherit their mutation's category.")))
    (when (eq class 'self-exercise-tool)
      (setf (gethash "verification-scope" properties)
            (json-object "type" "string" "enum" #("focused" "full")
                         "description" "The explicit scope of this check; defaults to focused.")))
    specification))

(-> default-tools--register-self (tool-registry) tool-registry)
(defun default-tools--register-self (registry)
  "Register active-image inspection and mutation tools in REGISTRY."
  (tool-registry-describe-namespace
   registry "self"
   "Operations on the active Autolith Common Lisp image.")
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'self-eval-tool
           "self" "eval"
           "Evaluate exploratory Common Lisp forms in order in the active image's AUTOLITH package. AUTOLITH: references accept internal symbols. Each form is read only after the one before it ran, so a later form may use a package an earlier form created. The result shows the last form's values; a failure stops the sequence and names the failing form. A restart reruns the whole sequence."
           (tool-object-schema
            (json-object
             "forms" (tool-forms-property
                      "The forms to run in order, each string holding exactly one form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("forms")))
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
           'self-redefine-tool
           "self" "define"
           "The same tool as self.redefine under a second name. The definition's name does not need to exist yet."
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
           'self-experiment-start-tool
           "self" "experiment-start"
           "Start one explicit tuning experiment for an effective pending self.set or self.redefine mutation."
           (tool-object-schema
            (json-object
             "mutation" (tool-string-property
                         "The exact effective mutation identifier from self.diff.")
             "hypothesis" (tool-string-property
                           "The nonempty hypothesis motivating this tuning change.")
             "criterion" (tool-string-property
                          "The nonempty expected effect and measurement criterion."))
            '("mutation" "hypothesis" "criterion")))
          (list
           'self-experiment-settle-tool
           "self" "experiment-settle"
           "Record evidence-based settlement for the open tuning experiment without committing or discarding its mutation."
           (tool-object-schema
            (json-object
             "experiment" (tool-string-property
                           "The open experiment identifier; defaults to the sole open experiment.")
             "verdict" (json-object
                        "type" "string"
                        "enum" #("better" "worse" "unchanged" "too-early")
                        "description" "The measured tuning outcome.")
             "observation" (tool-string-property
                            "A nonempty bounded observation supporting the verdict."))
            '("verdict" "observation")))
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
           "Evaluate and journal one focused assertion-style Common Lisp form against the active image. It defaults to the newest effective pending mutation, or verifies the current active state when none is pending. A signaled condition fails the exercise; this does not replace self.commit's full checks."
           (tool-object-schema
            (json-object
             "form" (tool-string-property
                     "One Common Lisp exercise form; use ASSERT or ERROR to signal failure.")
             "mutation" (tool-string-property
                         "An optional effective mutation identifier from self.diff; defaults to the newest effective mutation, or omit it to verify current state after discard."))
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
      (default-tools--register registry (default-tools--self-repair-schema specification))))
  registry)

(-> default-tools--remove-mutable-self-tools (tool-registry) tool-registry)
(defun default-tools--remove-mutable-self-tools (registry)
  "Remove every mutable active-image tool from REGISTRY."
  (tool-registry-delete-if registry
                           (lambda (tool)
                             (typep tool 'mutable-self-tool))))

(-> make-default-tool-registry
    (&key (:immutable-p boolean) (:configuration (option configuration)))
    tool-registry)
(defun make-default-tool-registry (&key immutable-p configuration)
  "Create Autolith's tool registry, omitting mutable self tools when requested.

LSP tools register only when CONFIGURATION enables a language server, and the
emacs.* tools and emacs: resources only when it names a reachable Emacs server
socket."
  (let ((registry (make-instance 'tool-registry))
        (search-worker (search-worker-create :configuration configuration)))
    (default-tools--register-workspace registry
                                       :knowledge-p (knowledge-active-p configuration))
    (default-tools--register-web registry)
    (default-tools--register-search registry search-worker)
    (default-tools--register-shell registry)
    (default-tools--register-papercut registry)
    (default-tools--register-agenda registry)
    (default-tools--register-plan registry
                                  :knowledge-p (knowledge-active-p configuration))
    (when (knowledge-active-p configuration)
      (default-tools--register-procedure registry))
    (when (and configuration
               (knowledge-active-p configuration)
               (config :diagnosis-p configuration))
      (default-tools--register-diagnosis registry))
    (default-tools--register-lisp registry)
    (when (and configuration (lsp-configuration-enabled-p configuration))
      (lsp-register-tools registry))
    (when (emacs-server-available-p configuration)
      (emacs-register-tools registry))
    (default-tools--register-self registry)
    (refinement-register-tools registry)
    (rlm-register-tools registry)
    (mission-register-tools registry)
    (mission-review-augment-tool-registry registry)
    (mission-schedule-register-tools registry)
    (skill-augment-tool-registry registry)
    (executable-skill-register-tools registry)
    (skill-edit-augment-tool-registry registry)
    (when (fboundp 'structural-register-default-tools)
      (structural-register-default-tools registry))
    (when (fboundp 'debug-register-default-tools)
      (debug-register-default-tools registry))
    (when immutable-p
      (default-tools--remove-mutable-self-tools registry))
    registry))
