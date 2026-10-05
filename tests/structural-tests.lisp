(in-package #:autolith)

;;;; -- Optional Structural Product Boundary Tests --

(-> structural-tests--program () string)
(defun structural-tests--program ()
  "Require an explicitly configured real backend for the optional integration suite."
  (or (uiop:getenv "CLASTED_AST_GREP") *structural-program*
      (error "Set CLASTED_AST_GREP to a real ast-grep executable for structural integration tests.")))

(-> structural-tests--write (pathname string) pathname)
(defun structural-tests--write (path text)
  "Write exact UTF-8 fixture text."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction ':output :if-exists ':supersede
                              :if-does-not-exist ':create :external-format ':utf-8)
    (write-string text stream))
  path)

(-> structural-tests--fixture (function) t)
(defun structural-tests--fixture (function)
  "Run FUNCTION against an isolated ordinary workspace with explicitly approved backend execution."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (make-instance 'tool-registry))
           (commands nil)
           (context (make-instance
                     'tool-context :configuration configuration :registry registry :worker nil
                     :conversation (conversation-create configuration :identifier "structural")
                     :command-authorization-function
                     (lambda (command directory)
                       (declare (ignore directory))
                       (push command commands)
                       ':full-access))))
      (default-tools--register-workspace registry)
      (structural-register-tools registry :program (structural-tests--program))
      (unwind-protect
           (funcall function configuration registry context (lambda () commands))
        (tool-registry-close-runtime-state registry)))))

(-> structural-tests--call (tool-context string json-object) json-object)
(defun structural-tests--call (context operation arguments)
  "Invoke the registered tool and decode its bounded JSON result."
  (let ((result (tool-registry-execute-call
                 (tool-context-registry context)
                 (json-object "namespace" "structural" "name" operation
                              "arguments" (json-encode arguments)) context)))
    (test-assert (tool-result-success-p result) (format nil "structural invocation: ~A" (tool-result-content result)))
    (json-decode (tool-result-content result))))

(-> structural-tests--revision (tool-context string) string)
(defun structural-tests--revision (context uri)
  "Observe a workspace URI and retain its ordinary conversation-local revision handle."
  (let* ((resource (resource-registry-resolve
                    (tool-registry-resource-registry (tool-context-registry context)) uri context))
         (observation (workspace-file--call-with-authorized-access
                       resource context ':read (lambda () (resource-observe resource context)))))
    (resource-observation-state-alias
     (resource-observation-state-ensure (tool-context-conversation context) observation :visible-ranges nil))))

(-> structural-tests--arguments (string &key (:pattern string) (:replacement (option string))) json-object)
(defun structural-tests--arguments (revision &key (pattern "foo($A)") replacement)
  "Build a request against a previously observed fixture revision."
  (let ((arguments (json-object "uri" "workspace:source.js" "base-revision" revision
                                "language" "javascript" "pattern" pattern)))
    (when replacement (setf (gethash "replacement" arguments) replacement))
    arguments))

(-> structural-tests--rejected (function) boolean)
(defun structural-tests--rejected (function)
  "Recognize typed structural or ordinary resource failures."
  (handler-case (progn (funcall function) nil)
    (tool-error () t)
    (cl-resources:resource-error () t)))

(-> test-structural-runtime-capability () null)
(defun test-structural-runtime-capability ()
  "Exercise optional registration, provider lookup, disabled runtime and required arguments."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore configuration commands))
     (let ((status (structural-tests--call context "status" (json-object))))
       (test-assert (eq t (json-get status "configured")) "explicit backend capability"))
     (test-assert (plusp (length (tool-registry-provider-schemas registry))) "optional schemas project")
     (structural-register-tools registry :program nil)
     (test-assert (not (eq t (json-get (structural-tests--call context "status" (json-object)) "configured")))
                  "disabled runtime is observable")
     (let ((result (tool-registry-execute-call
                    registry (json-object "namespace" "structural" "name" "query" "arguments" "{}") context)))
       (test-assert (not (tool-result-success-p result)) "missing revision and query arguments fail at invocation"))
     (let ((tool (tool-registry-find registry "structural" "query")))
       (test-assert
        (handler-case
            (progn (tool-execute tool context (structural-tests--arguments "unobserved")) nil)
          (structural-workspace-error (condition)
            (eq ':unavailable (structural-workspace-error-code condition))))
        "disabled operation signals typed availability failure"))
     (let ((result (tool-registry-execute-call
                    registry (json-object "namespace" "structural" "name" "query"
                                          "arguments" (json-encode (structural-tests--arguments "unobserved"))) context)))
       (test-assert (eq ':unavailable (tool-result-error-code result))
                    "structural failure code survives ordinary invocation"))))
  nil)

(-> test-structural-real-query-and-publication () null)
(defun test-structural-real-query-and-publication ()
  "Use real ast-grep with Unicode, stable match identities, preview staging and resource publication."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore registry))
     (let* ((path (structural-tests--write
                   (merge-pathnames "source.js" (config :working-directory configuration))
                   "const emoji = '😀'; foo(1); foo(2);"))
            (revision (structural-tests--revision context "workspace:source.js"))
            (arguments (structural-tests--arguments revision))
            (first (structural-tests--call context "query" arguments))
            (second (structural-tests--call context "query" arguments))
            (matches (json-get first "matches")))
       (test-assert (= 2 (length matches)) "real structural matches")
       (test-assert (equal (json-get first "snapshot") (json-get second "snapshot")) "snapshot stable for exact revision")
       (test-assert (equal (json-get (aref matches 0) "match")
                           (json-get (aref (json-get second "matches") 0) "match")) "match identity stable")
       (test-assert (> (json-get (aref matches 0) "startByte") (json-get (aref matches 0) "start"))
                    "UTF-8 span differs from character span")
       (test-assert (equal "foo(1)" (json-get (aref matches 0) "text")) "validated source match")
       (let* ((proposal (structural-tests--call context "rewrite"
                                               (structural-tests--arguments revision :replacement "bar($A)")))
              (identifier (json-get proposal "proposal")))
         (test-assert (equal "const emoji = '😀'; foo(1); foo(2);" (uiop:read-file-string path))
                      "preview stages without publication")
         (test-assert (equal "const emoji = '😀'; bar(1); bar(2);" (json-get proposal "preview"))
                      "complete real rewrite preview")
         (test-assert (= 2 (length (json-get proposal "edits"))) "ordered edit list exposed")
         (test-assert (equal (json-get proposal "preview")
                             (json-get (structural-tests--call context "inspect" (json-object "proposal" identifier)) "preview"))
                      "inspect retained preview")
         (test-assert (= 1 (json-get (structural-tests--call context "apply" (json-object "proposal" identifier)) "resources"))
                      "publication through staged resources")
         (test-assert (equal (json-get proposal "preview") (uiop:read-file-string path)) "published exact preview")
         (test-assert (structural-tests--rejected
                       (lambda () (structural--execute
                                   (tool-registry-find (tool-context-registry context) "structural" "apply")
                                   context (json-object "proposal" identifier)))) "successful proposal consumed"))
       (test-assert (some (lambda (command) (search "--stdin" command)) (funcall commands))
                    "actual backend argv passed through command authorization"))))
  nil)

(-> test-structural-stale-and-proposal-scope () null)
(defun test-structural-stale-and-proposal-scope ()
  "Reject stale publication and querying, cross-conversation proposals and bounded-state eviction."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore commands))
     (let* ((path (structural-tests--write (merge-pathnames "source.js" (config :working-directory configuration)) "foo(1);"))
            (revision (structural-tests--revision context "workspace:source.js"))
            (proposal (structural-tests--call context "rewrite" (structural-tests--arguments revision :replacement "bar($A)")))
            (identifier (json-get proposal "proposal"))
            (apply-tool (tool-registry-find registry "structural" "apply"))
            (other (make-instance 'tool-context :configuration configuration :registry registry :worker nil
                                  :conversation (conversation-create configuration :identifier "other"))))
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute apply-tool other (json-object "proposal" identifier))))
                    "proposal scoped to originating conversation")
       (structural-tests--write path "changed(1);")
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute apply-tool context (json-object "proposal" identifier)))) "stale apply rejected")
       (test-assert (equal "changed(1);" (uiop:read-file-string path)) "stale apply publishes nothing")
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute (tool-registry-find registry "structural" "query") context
                                              (structural-tests--arguments revision)))) "stale query rejected")
       (test-assert (equal identifier (json-get (structural-tests--call context "inspect" (json-object "proposal" identifier)) "proposal"))
                    "failed application retains inspection evidence")
       (let* ((*structural-maximum-proposals* 1)
              (new-revision (structural-tests--revision context "workspace:source.js"))
              (new (structural-tests--call context "rewrite"
                                          (structural-tests--arguments new-revision :pattern "changed($A)" :replacement "next($A)"))))
         (test-assert (structural-tests--rejected
                       (lambda () (tool-execute (tool-registry-find registry "structural" "inspect") context
                                                (json-object "proposal" identifier)))) "oldest proposal evicted")
         (test-assert (equal (json-get new "proposal")
                             (json-get (structural-tests--call context "inspect" (json-object "proposal" (json-get new "proposal"))) "proposal"))
                      "new bounded proposal retained")))))
  nil)

(-> test-structural-authorization-cancellation-errors () null)
(defun test-structural-authorization-cancellation-errors ()
  "Exercise denied backend commands, cancellation, backend errors and resource bounds."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore commands))
     (let* ((path (structural-tests--write (merge-pathnames "source.js" (config :working-directory configuration)) "foo(1);"))
            (revision (structural-tests--revision context "workspace:source.js"))
            (arguments (structural-tests--arguments revision :replacement "bar($A)"))
            (query (tool-registry-find registry "structural" "query")))
       (dolist (decision '(:deny :sandboxed))
         (let ((denied (make-instance
                        'tool-context :configuration configuration :registry registry :worker nil
                        :conversation (tool-context-conversation context)
                        :command-authorization-function
                        (lambda (command directory) (declare (ignore command directory)) decision))))
           (test-assert (structural-tests--rejected (lambda () (tool-execute query denied arguments)))
                        "unsandboxed backend requires full-access approval")))
       (structural-register-tools registry :program (structural-tests--program) :cancelled-p (lambda () t))
       (test-assert
        (handler-case
            (progn (tool-execute (tool-registry-find registry "structural" "rewrite") context arguments) nil)
          (structural-workspace-error (condition)
            (eq ':cancelled (structural-workspace-error-code condition)))) "cancelled backend retains no proposal")
       (structural-register-tools registry :program (structural-tests--program))
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute query context
                                              (structural-tests--arguments revision :pattern "foo(")))) "invalid backend pattern rejected")
       (let ((*structural-maximum-bytes* 2))
         (test-assert (structural-tests--rejected (lambda () (tool-execute query context arguments))) "bounded source snapshot"))
       (structural-register-tools registry :program (uiop:native-namestring (merge-pathnames "missing-ast-grep" (config :working-directory configuration))))
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute (tool-registry-find registry "structural" "query") context arguments))) "missing executable rejected")
       (test-assert (equal "foo(1);" (uiop:read-file-string path)) "failed operations publish no source"))))
  nil)

(-> test-structural-no-match-and-apply-cancellation () null)
(defun test-structural-no-match-and-apply-cancellation ()
  "Verify no-match plans and cancellation before the ordinary publication boundary."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore commands))
     (let* ((path (structural-tests--write (merge-pathnames "source.js" (config :working-directory configuration)) "foo(1);"))
            (revision (structural-tests--revision context "workspace:source.js"))
            (proposal (structural-tests--call context "rewrite"
                                               (structural-tests--arguments revision :pattern "missing($A)" :replacement "bar($A)")))
            (identifier (json-get proposal "proposal"))
            (apply-tool (tool-registry-find registry "structural" "apply"))
            (entry (structural--find (structural-tool-state apply-tool) context identifier)))
       (test-assert (zerop (length (json-get proposal "edits"))) "no-match rewrite has empty edit plan")
       (test-assert (structural-tests--rejected
                     (lambda () (structural--apply (structural-tool-state apply-tool) context entry :cancelled-p (lambda () t))))
                    "publication cancellation rejected before staging")
       (test-assert (equal "foo(1);" (uiop:read-file-string path)) "cancelled apply writes nothing")
       (test-assert (= 0 (json-get (structural-tests--call context "apply" (json-object "proposal" identifier)) "resources"))
                    "no-match apply validates and consumes without publishing")
       (let* ((new (structural-tests--call context "rewrite"
                                          (structural-tests--arguments revision :pattern "missing($A)" :replacement "bar($A)")))
              (new-id (json-get new "proposal")))
         (structural-tests--write path "changed(1);")
         (test-assert (structural-tests--rejected
                       (lambda () (tool-execute apply-tool context (json-object "proposal" new-id))))
                      "no-match apply checks stale original too")))))
  nil)


(-> test-structural-external-path-authority () null)
(defun test-structural-external-path-authority ()
  "Authorize out-of-root source, executable and publication paths independently through ordinary boundaries."
  (structural-tests--fixture
   (lambda (configuration registry original commands)
     (declare (ignore commands))
     (let* ((root (config :working-directory configuration))
            (workspace (merge-pathnames "workspace/" root))
            (configuration (configuration-copy configuration :working-directory workspace :source-root workspace))
            (source (structural-tests--write (merge-pathnames "external.js" root) "foo(1);"))
            (uri (concatenate 'string "workspace:" (workspace-file--encode-identifier (uiop:native-namestring source))))
            (phase ':allow)
            (requests nil)
            (context (make-instance
                      'tool-context :configuration configuration :registry registry :worker nil
                      :conversation (tool-context-conversation original)
                      :command-authorization-function
                      (lambda (command directory)
                        (declare (ignore directory))
                        (push command requests)
                        (if (or (and (eq phase ':deny-read) (uiop:string-prefix-p "resource.read --" command))
                                (and (eq phase ':deny-program) (uiop:string-prefix-p "structural --" command))
                                (and (eq phase ':deny-edit) (uiop:string-prefix-p "resource.edit --" command)))
                            ':deny ':full-access)))))
       (ensure-directories-exist workspace)
       (let* ((revision (structural-tests--revision context uri))
              (arguments (structural-tests--arguments revision :replacement "bar($A)"))
              (query (tool-registry-find registry "structural" "query"))
              (rewrite (tool-registry-find registry "structural" "rewrite"))
              (apply-tool (tool-registry-find registry "structural" "apply")))
         (setf (gethash "uri" arguments) uri
               phase ':deny-read)
         (test-assert (structural-tests--rejected (lambda () (tool-execute query context arguments)))
                      "external source needs ordinary read approval")
         (setf phase ':deny-program)
         (test-assert (structural-tests--rejected (lambda () (tool-execute query context arguments)))
                      "external executable path needs structural path approval")
         (test-assert (not (some (lambda (command) (search "--stdin" command)) requests))
                      "path denials happen before backend invocation")
         (setf phase ':allow)
         (test-assert (= 1 (length (json-get (structural-tests--call context "query" arguments) "matches")))
                      "approved external source and program queried")
         (setf phase ':deny-edit)
         (test-assert (structural-tests--rejected (lambda () (tool-execute rewrite context arguments)))
                      "preview staging needs ordinary edit approval")
         (setf phase ':allow)
         (let* ((proposal (structural-tests--call context "rewrite" arguments))
                (identifier (json-get proposal "proposal")))
           (setf phase ':deny-edit)
           (test-assert (structural-tests--rejected
                         (lambda () (tool-execute apply-tool context (json-object "proposal" identifier))))
                        "publication reauthorizes external edit path")
           (test-assert (equal "foo(1);" (uiop:read-file-string source)) "denied preview and apply publish nothing")
           (setf phase ':allow)
           (structural-tests--call context "apply" (json-object "proposal" identifier))
           (test-assert (equal "bar(1);" (uiop:read-file-string source)) "ordinary approved external publication"))
         (test-assert (every (lambda (prefix)
                               (some (lambda (command) (uiop:string-prefix-p prefix command)) requests))
                             '("resource.read --" "resource.edit --" "structural --"))
                      "all path approvals use their ordinary operation names")))))
  nil)

(-> test-structural-expired-resource-revision () null)
(defun test-structural-expired-resource-revision ()
  "Retained structural previews cannot extend an expired ordinary resource revision's lifetime."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore commands))
     (let* ((*workspace-file-resource-maximum-observations* 1)
            (root (config :working-directory configuration))
            (source (structural-tests--write (merge-pathnames "source.js" root) "foo(1);"))
            (revision (structural-tests--revision context "workspace:source.js"))
            (proposal (structural-tests--call context "rewrite"
                                               (structural-tests--arguments revision :replacement "bar($A)"))))
       (structural-tests--write (merge-pathnames "other.js" root) "other(1);")
       (structural-tests--revision context "workspace:other.js")
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute (tool-registry-find registry "structural" "apply") context
                                              (json-object "proposal" (json-get proposal "proposal")))))
                    "expired alias cannot be refreshed from a retained structural snapshot")
       (test-assert (structural-tests--rejected
                     (lambda () (tool-execute (tool-registry-find registry "structural" "query") context
                                              (structural-tests--arguments revision)))) "expired query revision rejected")
       (test-assert (equal "foo(1);" (uiop:read-file-string source)) "expired proposal publishes nothing")
       (let* ((new-revision (structural-tests--revision context "workspace:source.js"))
              (query (structural-tests--call context "query" (structural-tests--arguments new-revision))))
         (test-assert (not (equal (json-get proposal "snapshot") (json-get query "snapshot")))
                      "new revision produces a distinct snapshot identity despite equal source")
         (test-assert (= 1 (length (json-get query "matches"))) "reread revision supports a fresh query")))))
  nil)


(-> test-structural-concurrent-proposal-application () null)
(defun test-structural-concurrent-proposal-application ()
  "Reserve no-match applications atomically and release failed attempts for ordinary inspection and retry."
  (structural-tests--fixture
   (lambda (configuration registry context commands)
     (declare (ignore commands))
     (let* ((source (structural-tests--write
                     (merge-pathnames "source.js" (config :working-directory configuration)) "foo(1);"))
            (revision (structural-tests--revision context "workspace:source.js"))
            (arguments (structural-tests--arguments revision :pattern "missing($A)" :replacement "bar($A)"))
            (proposal (structural-tests--call context "rewrite" arguments))
            (identifier (json-get proposal "proposal"))
            (call (json-object "namespace" "structural" "name" "apply"
                               "arguments" (json-encode (json-object "proposal" identifier))))
            (first-context (make-instance 'tool-context :configuration configuration :registry registry :worker nil
                                          :conversation (tool-context-conversation context)))
            (entered (sb-thread:make-semaphore :count 0))
            (release (sb-thread:make-semaphore :count 0))
            (original (symbol-function 'cl-resources:resource-check-revision))
            (thread nil)
            (first-result nil))
       (test-call-with-function-replacements
        (list
         (list 'cl-resources:resource-check-revision
               (lambda (resource request-context expected)
                 (when (eq request-context first-context)
                   (sb-thread:signal-semaphore entered)
                   (unless (sb-thread:wait-on-semaphore release :timeout 10)
                     (structural--fail ':test-timeout "Concurrent fixture release timed out.")))
                 (funcall original resource request-context expected))))
        (lambda ()
          (unwind-protect
               (progn
                 (setf thread
                       (sb-thread:make-thread
                        (lambda () (setf first-result (tool-registry-execute-call registry call first-context)))
                        :name "structural no-match apply"))
                 (test-assert (sb-thread:wait-on-semaphore entered :timeout 10)
                              "first invocation reached ordinary resource validation")
                 (let ((competing (tool-registry-execute-call registry call context)))
                   (test-assert (not (tool-result-success-p competing)) "concurrent no-match invocation rejected")
                   (test-assert (eq ':proposal-busy (tool-result-error-code competing)) "competing apply has typed busy failure"))
                 (test-assert (equal identifier
                                     (json-get (structural-tests--call context "inspect" (json-object "proposal" identifier)) "proposal"))
                              "reserved proposal remains inspectable")
                 (let* ((*structural-maximum-proposals* 1)
                        (capacity (tool-registry-execute-call
                                   registry (json-object "namespace" "structural" "name" "rewrite"
                                                         "arguments" (json-encode arguments)) context)))
                   (test-assert (eq ':proposal-limit (tool-result-error-code capacity))
                                "bounded retention cannot evict an active application")))
            (sb-thread:signal-semaphore release)
            (when thread (sb-thread:join-thread thread)))))
       (test-assert (tool-result-success-p first-result) "reserved no-match invocation succeeds once")
       (test-assert (eq ':unknown-proposal (tool-result-error-code (tool-registry-execute-call registry call context)))
                    "successful application consumes its reservation")
       (let* ((retry (structural-tests--call context "rewrite" arguments))
              (retry-id (json-get retry "proposal"))
              (retry-call (json-object "namespace" "structural" "name" "apply"
                                       "arguments" (json-encode (json-object "proposal" retry-id)))))
         (test-call-with-function-replacements
          (list (list 'cl-resources:resource-check-revision
                      (lambda (resource request-context expected)
                        (declare (ignore resource request-context expected))
                        (structural--fail ':test-validation "Temporary resource validation failure."))))
          (lambda ()
            (test-assert (eq ':test-validation
                             (tool-result-error-code (tool-registry-execute-call registry retry-call context)))
                         "ordinary validation failure returns its typed cause")))
         (test-assert (equal retry-id
                             (json-get (structural-tests--call context "inspect" (json-object "proposal" retry-id)) "proposal"))
                      "failed reserved application retains inspection evidence")
         (test-assert (tool-result-success-p (tool-registry-execute-call registry retry-call context))
                      "failed application releases reservation for retry"))
       (test-assert (equal "foo(1);" (uiop:read-file-string source)) "concurrent and retried no-match applications write nothing"))))
  nil)

(define-test-suite structural
  test-structural-runtime-capability
  test-structural-real-query-and-publication
  test-structural-stale-and-proposal-scope
  test-structural-authorization-cancellation-errors
  test-structural-no-match-and-apply-cancellation
  test-structural-external-path-authority
  test-structural-expired-resource-revision
  test-structural-concurrent-proposal-application)
