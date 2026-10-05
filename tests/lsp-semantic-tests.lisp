(in-package #:autolith)

;;;; -- Semantic LSP Resource Integration Fixtures --

(-> lsp-semantic-tests--workspace (configuration) configuration)
(defun lsp-semantic-tests--workspace (configuration)
  "Use an owned workspace below this fixture's temporary configuration root."
  (let ((root (merge-pathnames "workspace/" (test-configuration-root configuration))))
    (ensure-directories-exist root)
    (configuration-copy configuration :working-directory root)))

(-> lsp-semantic-tests--write (configuration string string) pathname)
(defun lsp-semantic-tests--write (configuration name text)
  "Write exact UTF-8 fixture text inside the configured workspace."
  (let ((path (merge-pathnames name (config :working-directory configuration))))
    (workspace-resource-tests--write-text path text)
    path))

(-> lsp-semantic-tests--context
    (configuration tool-registry string &key (:authorization (option function))) tool-context)
(defun lsp-semantic-tests--context (configuration registry identifier &key authorization)
  "Create an isolated conversation context for semantic fixtures."
  (make-instance 'tool-context :configuration configuration :registry registry :worker nil
                 :conversation (conversation-create configuration :identifier identifier)
                 :command-authorization-function authorization))

(-> lsp-semantic-tests--proposal (tool-context lsp-client json-object) lsp-semantic-entry)
(defun lsp-semantic-tests--proposal (context client edit)
  "Normalize a raw server edit against authorized exact fixture snapshots."
  (let* ((snapshots (make-hash-table :test #'equal))
         (snapshot (lsp-semantic--snapshot-function context client snapshots)))
    (lsp-semantic--capture-plan
     context client :snapshots snapshots :plan (cl-lsp:lsp-normalize-workspace-edit edit :snapshot snapshot))))

(-> lsp-semantic-tests--text-edit (integer integer string) json-object)
(defun lsp-semantic-tests--text-edit (start end text)
  "Construct a single-line UTF-16 fixture edit."
  (json-object "range" (json-object "start" (json-object "line" 0 "character" start)
                                     "end" (json-object "line" 0 "character" end))
               "newText" text))

(-> lsp-semantic-tests--rejected (function) boolean)
(defun lsp-semantic-tests--rejected (thunk)
  "Return true when THUNK signals a typed resource, tool or LSP failure."
  (handler-case (progn (funcall thunk) nil)
    (lsp-error () t)
    (cl-resources:resource-error () t)
    (tool-error () t)))

(-> test-lsp-semantic-ordered-resources () null)
(defun test-lsp-semantic-ordered-resources ()
  "Apply ordered Unicode edits, creates, moves, deletions and repeated text operations."
  (with-test-configuration (configuration)
    (setf configuration (lsp-semantic-tests--workspace configuration))
    (let* ((registry (make-default-tool-registry :configuration configuration))
           (context (lsp-semantic-tests--context configuration registry "semantic-ordered"))
           (root (config :working-directory configuration))
           (client (lsp-client-tests--client root))
           (source (lsp-semantic-tests--write configuration "source.txt" "a😀z"))
           (removed (lsp-semantic-tests--write configuration "removed.txt" "remove"))
           (target (merge-pathnames "moved.txt" root))
           (intermediate (merge-pathnames "intermediate.txt" root))
           (created (merge-pathnames "created.txt" root))
           (source-uri (cl-lsp:lsp-path-uri source))
           (target-uri (cl-lsp:lsp-path-uri target))
           (created-uri (cl-lsp:lsp-path-uri created))
           (intermediate-uri (cl-lsp:lsp-path-uri intermediate))
           (edit (json-object
                  "documentChanges"
                  (vector
                   (json-object "textDocument" (json-object "uri" source-uri "version" nil)
                                "edits" (vector (lsp-semantic-tests--text-edit 1 3 "X")))
                   (json-object "kind" "rename" "oldUri" source-uri "newUri" intermediate-uri)
                   (json-object "kind" "rename" "oldUri" intermediate-uri "newUri" target-uri)
                   (json-object "textDocument" (json-object "uri" target-uri "version" nil)
                                "edits" (vector (lsp-semantic-tests--text-edit 1 2 "Y")))
                   (json-object "kind" "create" "uri" created-uri)
                   (json-object "textDocument" (json-object "uri" created-uri "version" nil)
                                "edits" (vector (lsp-semantic-tests--text-edit 0 0 "first")
                                                (lsp-semantic-tests--text-edit 0 0 "second")))
                   (json-object "kind" "delete" "uri" (cl-lsp:lsp-path-uri removed))))))
      (with-test-fixture (':file-modes "semantic move preserving source permissions")
        (test-fixture-set-file-mode *platform* source #o750))
      (unwind-protect
           (let ((entry (lsp-semantic-tests--proposal context client edit)))
             (test-assert (= 4 (length (lsp-semantic--apply context entry #()))) "four resources published")
             (test-assert (not (probe-file source)) "source removed through adapter")
             (test-assert (not (probe-file removed)) "deletion through adapter")
             (test-assert (equal "aYz" (uiop:read-file-string target)) "ordered moved content")
             (test-assert (equal "firstsecond" (uiop:read-file-string created)) "stable simultaneous insert order")
             (test-assert (not (probe-file intermediate)) "intermediate move has no residual file")
             (with-test-fixture (':file-modes "semantic multi-hop move permissions")
               (test-assert (= #o750 (test-fixture-file-mode *platform* target))
                            "edited multi-hop move preserves source permissions")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-lsp-semantic-preconditions () null)
(defun test-lsp-semantic-preconditions ()
  "Reject collisions and unsupported resource kinds, preserve conditional operation semantics."
  (with-test-configuration (configuration)
    (setf configuration (lsp-semantic-tests--workspace configuration))
    (let* ((registry (make-default-tool-registry :configuration configuration))
           (context (lsp-semantic-tests--context configuration registry "semantic-preconditions"))
           (root (config :working-directory configuration))
           (client (lsp-client-tests--client root))
           (source (lsp-semantic-tests--write configuration "source.txt" "source"))
           (target (lsp-semantic-tests--write configuration "target.txt" "target"))
           (missing (cl-lsp:lsp-path-uri (merge-pathnames "missing.txt" root)))
           (source-uri (cl-lsp:lsp-path-uri source))
           (target-uri (cl-lsp:lsp-path-uri target)))
      (unwind-protect
           (progn
             (dolist (operation
                      (list (json-object "kind" "create" "uri" target-uri)
                            (json-object "kind" "rename" "oldUri" source-uri "newUri" target-uri)
                            (json-object "kind" "delete" "uri" missing)
                            (json-object "kind" "delete" "uri" (cl-lsp:lsp-path-uri root))
                            (json-object "kind" "create" "uri" "https://example.org/file")
                            (json-object "kind" "create" "uri" "file://host/file")))
               (test-assert
                (lsp-semantic-tests--rejected
                 (lambda () (lsp-semantic-tests--proposal
                             context client (json-object "documentChanges" (vector operation)))))
                "invalid resource precondition fails before mutation"))
             (let ((entry (lsp-semantic-tests--proposal
                           context client
                           (json-object "documentChanges"
                                        (vector (json-object "kind" "create" "uri" target-uri
                                                             "options" (json-object "ignoreIfExists" t))
                                                (json-object "kind" "rename" "oldUri" source-uri "newUri" target-uri
                                                             "options" (json-object "ignoreIfExists" t))
                                                (json-object "kind" "delete" "uri" missing
                                                             "options" (json-object "ignoreIfNotExists" t)))))))
               (test-assert (null (lsp-semantic--apply context entry #())) "ignored operations do not publish")
               (test-assert (equal "source" (uiop:read-file-string source)) "ignored move retains source")
               (test-assert (equal "target" (uiop:read-file-string target)) "ignored collision retains target"))
             (let ((entry (lsp-semantic-tests--proposal
                           context client
                           (json-object "documentChanges"
                                        (vector (json-object "kind" "rename" "oldUri" source-uri "newUri" target-uri
                                                             "options" (json-object "overwrite" t)))))))
               (lsp-semantic--apply context entry #())
               (test-assert (equal "source" (uiop:read-file-string target)) "overwrite move preserves source text")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-lsp-semantic-stale-and-approval () null)
(defun test-lsp-semantic-stale-and-approval ()
  "Require approval and reject stale disk snapshots, exact document versions and restarted clients."
  (with-test-configuration (configuration)
    (setf configuration (lsp-semantic-tests--workspace configuration))
    (let* ((registry (make-default-tool-registry :configuration configuration))
           (confirmation-allowed nil)
           (confirmations nil)
           (context (lsp-semantic-tests--context
                     configuration registry "semantic-stale"
                     :authorization (lambda (command directory)
                                      (declare (ignore directory))
                                      (push command confirmations)
                                      (if confirmation-allowed ':full-access ':deny))))
           (root (config :working-directory configuration))
           (path (lsp-semantic-tests--write configuration "a.txt" "before"))
           (uri (cl-lsp:lsp-path-uri path))
           (document (make-instance 'lsp-document :path path :text "before"))
           (client (lsp-client-tests--client root))
           (change (lsp-semantic-tests--text-edit 0 6 "after")))
      (setf (gethash uri (cl-lsp:lsp-client-documents client)) document
            (gethash "annotationId" change) "confirm")
      (labels ((proposal ()
                 (lsp-semantic-tests--proposal
                  context client
                  (json-object "changeAnnotations" (json-object "confirm" (json-object "label" "Rename" "needsConfirmation" t))
                             "documentChanges" (vector (json-object "textDocument" (json-object "uri" uri "version" (cl-lsp:lsp-document-version document))
                                                                      "edits" (vector change)))))))
        (unwind-protect
             (progn
               (let ((entry (proposal)))
                 (test-assert (lsp-semantic-tests--rejected
                               (lambda () (lsp-semantic--apply context entry #()))) "confirmation required")
                 (test-assert (equal "before" (uiop:read-file-string path)) "approval failure writes nothing")
                 (test-assert (lsp-semantic-tests--rejected
                               (lambda () (lsp-semantic--apply context entry #("confirm")))) "caller assertion does not bypass permission")
                 (test-assert (some (lambda (command)
                                     (and (search (lsp-semantic-entry-identifier entry) command)
                                          (search "confirm" command))) confirmations)
                              "confirmation command includes proposal and annotation identities")
                 (setf confirmation-allowed t)
                 (incf (cl-lsp:lsp-document-version document))
                 (test-assert (lsp-semantic-tests--rejected
                               (lambda () (lsp-semantic--apply context entry #("confirm")))) "version drift rejected"))
               (let ((entry (proposal)))
                 (lsp-semantic-tests--write configuration "a.txt" "external")
                 (test-assert (lsp-semantic-tests--rejected
                               (lambda () (lsp-semantic--apply context entry #("confirm")))) "disk drift rejected")
                 (test-assert (equal "external" (uiop:read-file-string path)) "stale failure preserves external change"))
               (lsp-semantic-tests--write configuration "a.txt" "before")
               (let ((entry (proposal)))
                 (setf (lsp-client-transport client) (list :new))
                 (test-assert (lsp-semantic-tests--rejected
                               (lambda () (lsp-semantic--apply context entry #("confirm")))) "restart invalidates proposal"))
               (lsp-semantic--apply context (proposal) #("confirm"))
               (test-assert (equal "after" (uiop:read-file-string path)) "explicit approval publishes"))
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-lsp-semantic-conversation-bounds () null)
(defun test-lsp-semantic-conversation-bounds ()
  "Keep choices bounded and inaccessible across conversations."
  (with-test-configuration (configuration)
    (setf configuration (lsp-semantic-tests--workspace configuration))
    (let* ((registry (make-default-tool-registry :configuration configuration))
           (first (lsp-semantic-tests--context configuration registry "semantic-first"))
           (second (lsp-semantic-tests--context configuration registry "semantic-second"))
           (client (lsp-client-tests--client (config :working-directory configuration)))
           (state (make-instance 'lsp-semantic-state))
           (*lsp-semantic-maximum-entries* 1)
           (one (lsp-semantic-tests--proposal first client (json-object)))
           (two (lsp-semantic-tests--proposal first client (json-object)))
           (id (lsp-semantic--store state first one)))
      (unwind-protect
           (progn
             (test-assert (lsp-semantic-tests--rejected
                           (lambda () (lsp-semantic--find state second :identifier id :kind ':proposal))) "foreign conversation cannot use ID")
             (lsp-semantic--store state first two)
             (test-assert (lsp-semantic-tests--rejected
                           (lambda () (lsp-semantic--find state first :identifier id :kind ':proposal))) "oldest proposal evicted")
             (test-assert (eq two (lsp-semantic--find state first :identifier (lsp-semantic-entry-identifier two) :kind ':proposal)) "latest retained")
             (let* ((path (lsp-semantic-tests--write configuration "expired.txt" "old"))
                    (uri (cl-lsp:lsp-path-uri path))
                    (entry (lsp-semantic-tests--proposal
                            first client (json-object "changes"
                                                      (json-object uri (vector (lsp-semantic-tests--text-edit 0 3 "new"))))))
                    (alias (second (gethash uri (lsp-semantic-entry-snapshots entry)))))
               (fifo-cache-delete-first-if
                (lambda (candidate state)
                  (declare (ignore state))
                  (equal candidate alias))
                (conversation-resource-observations (tool-context-conversation first)))
               (test-assert (lsp-semantic-tests--rejected
                             (lambda () (lsp-semantic--apply first entry #()))) "expired resource revision cannot be applied")
               (test-assert (equal "old" (uiop:read-file-string path)) "expired proposal writes nothing"))
             (let ((*lsp-semantic-maximum-characters* 1))
               (test-assert (lsp-semantic-tests--rejected
                             (lambda () (lsp-semantic--store state first two))) "oversized choices rejected")))
        (tool-registry-close-runtime-state registry))))
  nil)


(-> test-lsp-semantic-tool-workflow () null)
(defun test-lsp-semantic-tool-workflow ()
  "Exercise preparation, discovery, resolution, proposal and application through tool dispatch."
  (with-test-configuration (configuration)
    (setf configuration (lsp-semantic-tests--workspace configuration))
    (let* ((registry (make-default-tool-registry :configuration configuration))
           (context (lsp-semantic-tests--context configuration registry "semantic-tools"))
           (root (config :working-directory configuration))
           (path (lsp-semantic-tests--write configuration "a.txt" "old"))
           (uri (cl-lsp:lsp-path-uri path))
           (document (make-instance 'lsp-document :path path :text "old"))
           (client (lsp-client-tests--client
                    root :capabilities
                    (json-object "renameProvider" (json-object "prepareProvider" t)
                                 "codeActionProvider" (json-object "resolveProvider" t)
                                 "workspace" (json-object "fileOperations"
                                                          (json-object "willRename"
                                                                       (json-object "filters"
                                                                                    (vector (json-object "scheme" "file"
                                                                                                         "pattern" (json-object "glob" "**/*.txt")))))))))
           (raw-edit (json-object "changes" (json-object uri (vector (lsp-semantic-tests--text-edit 0 3 "new")))))
           (observed nil))
      (setf (gethash uri (cl-lsp:lsp-client-documents client)) document)
      (lsp-register-tools registry)
      (labels ((call (&rest arguments)
                 (apply #'workspace-resource-tests--call registry context "lsp" "semantic" arguments))

               (value (&rest arguments)
                 (let ((result (apply #'call arguments)))
                   (test-assert (tool-result-success-p result)
                                (format nil "Semantic tool result: ~A" (tool-result-content result)))
                   (json-decode (tool-result-content result)))))
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'lsp-tool--call-for-file
                     (lambda (manager context selected function)
                       (declare (ignore manager context selected))
                       (vector (json-object "server" "fixture" "result" (funcall function client document)))))
               (list 'lsp-transport-request
                     (lambda (transport method params &key timeout)
                       (declare (ignore transport timeout))
                       (push (cons method params) observed)
                       (cond
                         ((equal method "textDocument/prepareRename")
                          (json-object "range" (json-get (lsp-semantic-tests--text-edit 0 3 "") "range") "placeholder" "old"))
                         ((equal method "textDocument/rename")
                          raw-edit)
                         ((equal method "textDocument/codeAction")
                          (vector (json-object "title" "Resolve me" "kind" "quickfix" "isPreferred" t "data" (json-object "opaque" 7))
                                  (json-object "title" "Command" "command" "fixture.command")
                                  (json-object "title" "Disabled" "disabled" (json-object "reason" "blocked") "edit" raw-edit)))
                         ((equal method "codeAction/resolve")
                          (test-assert (= 7 (json-get (json-get params "data") "opaque")) "opaque resolution data preserved")
                          (json-object "title" "Resolved" "edit" raw-edit))
                         ((equal method "workspace/willRenameFiles")
                          (json-object "changes" (json-object uri (vector (lsp-semantic-tests--text-edit 0 3 "ready")))))
                         (t
                          (error "Unexpected fixture method ~A." method))))))
              (lambda ()
                (setf (gethash "positionEncoding" (lsp-client-capabilities client)) "utf-8")
                (test-assert (not (tool-result-success-p
                                   (call "operation" "prepare-rename" "path" (namestring path) "line" 1 "character" 1)))
                             "unsupported negotiated encoding rejects preparation")
                (test-assert (null observed) "encoding guard precedes server requests")
                (setf (gethash "positionEncoding" (lsp-client-capabilities client)) "utf-16")
                (let ((prepared (json-get (aref (value "operation" "prepare-rename" "path" (namestring path) "line" 1 "character" 1) 0) "result")))
                  (test-assert (= 3 (json-get prepared "endOffset")) "symbol preparation offsets exposed"))
                (let* ((rename (json-get (aref (value "operation" "rename" "path" (namestring path) "line" 1 "character" 1 "new-name" "new") 0) "result"))
                       (id (json-get rename "proposal")))
                  (test-assert (equal "old" (uiop:read-file-string path)) "rename is only a proposal")
                  (test-assert (= 1 (length (json-get (value "operation" "inspect" "proposal" id) "operations"))) "proposal inspect returns operations"))
                (let* ((choices (json-get (aref (value "operation" "code-actions" "path" (namestring path)
                                                       "line" 1 "character" 1 "end-line" 1 "end-character" 4) 0) "result"))
                       (resolved (value "operation" "resolve" "choice" (json-get (aref choices 0) "choice")))
                       (proposal (value "operation" "propose" "choice" (json-get resolved "choice")))
                       (id (json-get proposal "proposal")))
                  (test-assert (json-get (json-get (aref choices 0) "action") "isPreferred") "preferred action metadata presented")
                  (test-assert (not (tool-result-success-p (call "operation" "propose" "choice" (json-get (aref choices 1) "choice")))) "command-only action cannot become an edit")
                  (test-assert (not (tool-result-success-p (call "operation" "propose" "choice" (json-get (aref choices 2) "choice")))) "disabled action rejected")
                  (value "operation" "apply" "proposal" id)
                  (test-assert (equal "new" (uiop:read-file-string path)) "resolved action applied through resources")
                  (test-assert (not (tool-result-success-p (call "operation" "apply" "proposal" id))) "successful proposal consumed"))
                (setf (lsp-document-text document) "new")
                (incf (cl-lsp:lsp-document-version document))
                (let* ((destination (merge-pathnames "moved.txt" root))
                       (move (json-get (aref (value "operation" "move" "path" (namestring path) "new-path" (namestring destination)) 0) "result")))
                  (test-assert (probe-file path) "move preparation writes nothing")
                  (value "operation" "apply" "proposal" (json-get move "proposal"))
                  (test-assert (not (probe-file path)) "move source deleted via adapter")
                  (test-assert (equal "ready" (uiop:read-file-string destination)) "preparatory edits precede the physical move"))
                (test-assert (not (assoc "workspace/executeCommand" observed :test #'equal)) "server commands are never executed")))
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-lsp-semantic-authority-and-validation () null)
(defun test-lsp-semantic-authority-and-validation ()
  "Authorize outside paths before observation and again at apply; reject encoding/version/range drift."
  (with-test-configuration (configuration)
    (setf configuration (lsp-semantic-tests--workspace configuration))
    (let* ((registry (make-default-tool-registry :configuration configuration))
           (context (lsp-semantic-tests--context configuration registry "semantic-authority"))
           (root (config :working-directory configuration))
           (client (lsp-client-tests--client root))
           (outside (merge-pathnames "outside.txt" (test-configuration-root configuration)))
           (uri (cl-lsp:lsp-path-uri outside))
           (edit (json-object "changes" (json-object uri (vector (lsp-semantic-tests--text-edit 0 3 "new")))))
           (authorizations 0)
           (allowed nil))
      (workspace-resource-tests--write-text outside "old")
      (unwind-protect
           (test-call-with-function-replacements
            (list (list 'workspace-tool-authorize-outside-path
                        (lambda (context path tool-name)
                          (declare (ignore context path tool-name))
                          (incf authorizations)
                          allowed)))
            (lambda ()
              (test-assert (lsp-semantic-tests--rejected
                            (lambda () (lsp-semantic-tests--proposal context client edit))) "outside read denied before proposal")
              (test-assert (plusp authorizations) "ordinary path authority consulted")
              (setf allowed t)
              (let ((entry (lsp-semantic-tests--proposal context client edit)))
                (setf allowed nil)
                (test-assert (lsp-semantic-tests--rejected
                              (lambda () (lsp-semantic--apply context entry #()))) "outside write reauthorized")
                (test-assert (equal "old" (uiop:read-file-string outside)) "denied application preserves content")
                (setf allowed t)
                (lsp-semantic--apply context entry #())
                (test-assert (equal "new" (uiop:read-file-string outside)) "explicit outside authorization permits revisioned write"))
              (let* ((second (lsp-semantic-tests--write configuration "second.txt" "old"))
                     (second-uri (cl-lsp:lsp-path-uri second))
                     (entry (lsp-semantic-tests--proposal
                             context client
                             (json-object "changes"
                                          (json-object uri (vector (lsp-semantic-tests--text-edit 0 3 "bad"))
                                                       second-uri (vector (lsp-semantic-tests--text-edit 0 3 "bad")))))))
                (lsp-semantic-tests--write configuration "second.txt" "external")
                (test-assert (lsp-semantic-tests--rejected
                              (lambda () (lsp-semantic--apply context entry #()))) "one stale member rejects the complete proposal")
                (test-assert (equal "new" (uiop:read-file-string outside)) "multi-resource validation precedes all writes"))
              (test-assert (lsp-semantic-tests--rejected
                            (lambda () (cl-lsp:lsp-normalize-workspace-edit edit :position-encoding "utf-8"))) "unsupported encoding rejected")
              (let ((snapshot (lambda (uri) (declare (ignore uri)) (values "new" 2 ':file))))
                (test-assert (lsp-semantic-tests--rejected
                              (lambda () (cl-lsp:lsp-normalize-workspace-edit
                                          (json-object "documentChanges"
                                                       (vector (json-object "textDocument" (json-object "uri" uri "version" 1)
                                                                            "edits" (vector (lsp-semantic-tests--text-edit 0 3 "x")))))
                                          :snapshot snapshot))) "explicit stale document version rejected")
                (test-assert (lsp-semantic-tests--rejected
                              (lambda () (cl-lsp:lsp-normalize-workspace-edit
                                          (json-object "changes" (json-object uri (vector (lsp-semantic-tests--text-edit 0 99 "x"))))
                                          :snapshot snapshot))) "invalid UTF-16 range rejected"))))
        (tool-registry-close-runtime-state registry))))
  nil)
