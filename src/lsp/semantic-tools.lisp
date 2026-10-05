(in-package #:autolith)

;;;; -- Semantic LSP Tool Surface --

(defclass lsp-semantic-tool (lsp-tool)
  ((state :initarg :state :reader lsp-semantic-tool-state
          :documentation "Registry-owned bounded conversation choices."))
  (:documentation "Prepare semantic changes, inspect choices and apply approved revisioned proposals."))

(-> lsp-semantic--present (lsp-semantic-state tool-context lsp-semantic-entry) json-object)
(defun lsp-semantic--present (state context entry)
  "Return a retained proposal identifier and its complete untrusted change description."
  (json-object "proposal" (lsp-semantic--store state context entry)
               "changes" (lsp-semantic-entry-value entry)
               "approval" "Pass approved annotation IDs explicitly to apply after obtaining approval."))

(-> lsp-semantic--range (lsp-document json-object) json-object)
(defun lsp-semantic--range (document arguments)
  "Construct a protocol range from one-based UTF-16 input coordinates."
  (let ((end (tool-argument arguments "end-line" :required t))
        (column (tool-argument arguments "end-character" :required t)))
    (unless (and (integerp end) (plusp end) (integerp column) (plusp column))
      (lsp-semantic--fail "Code-action range end coordinates must be positive integers."))
    (json-object "start" (lsp-tool--position document arguments)
                 "end" (lsp-text-position (lsp-document-text document) (1- end) (1- column)))))

(-> lsp-semantic--action-choice
    (lsp-semantic-state tool-context &key (:client lsp-client) (:snapshots hash-table)
                                         (:row json-object)) json-object)
(defun lsp-semantic--action-choice (state context &key client snapshots row)
  "Retain a server action and expose its metadata without executing commands."
  (when (json-get row "edit")
    (lsp-semantic--capture-plan context client :snapshots snapshots :plan (json-get row "edit")))
  (let ((entry (make-instance 'lsp-semantic-entry :client client
                              :transport (lsp-client-transport client) :kind ':action
                              :value row :snapshots snapshots)))
    (json-object "choice" (lsp-semantic--store state context entry)
                 "action" (json-get row "action")
                 "changes" (json-get row "edit")
                 "command" (json-get row "command")
                 "resolveSupported" (json-get row "resolveSupported"))))

(-> lsp-semantic--file-request (lsp-semantic-tool tool-context json-object) vector)
(defun lsp-semantic--file-request (tool context arguments)
  "Run one preparation/discovery operation independently for matching configured servers."
  (let* ((path (lsp-tool--path context (tool-argument arguments "path" :required t)))
         (operation (tool-argument arguments "operation" :required t))
         (state (lsp-semantic-tool-state tool)))
    (lsp-tool--call-for-file
     (lsp-tool-manager tool) context path
     (lambda (client document)
       (let* ((snapshots (make-hash-table :test #'equal))
              (snapshot (lsp-semantic--snapshot-function context client snapshots)))
         (unless (equal (json-get (lsp-client-capabilities client) "positionEncoding" "utf-16")
                        "utf-16")
           (lsp-semantic--fail "Semantic requests require negotiated UTF-16 positions."))
         ;; The synchronized source participates even when a server edits only other files.
         (funcall snapshot (cl-lsp:lsp-path-uri path))
         (cond
           ((equal operation "prepare-rename")
            (cl-lsp:lsp-client-prepare-rename client document (lsp-tool--position document arguments)))
           ((equal operation "rename")
            (lsp-semantic--present
             state context
             (lsp-semantic--capture-plan
              context client :snapshots snapshots
              :plan (cl-lsp:lsp-client-rename
               client document :position (lsp-tool--position document arguments)
               :new-name (tool-argument arguments "new-name" :required t) :snapshot snapshot))))
           ((equal operation "code-actions")
            (let ((rows (cl-lsp:lsp-client-code-actions
                         client document :range (lsp-semantic--range document arguments)
                         :diagnostics (or (tool-argument arguments "diagnostics") #())
                         :only (or (tool-argument arguments "only") #())
                         :snapshot snapshot)))
              (when (> (length rows) *lsp-semantic-maximum-entries*)
                (lsp-semantic--fail "Too many code actions; narrow the range or kind filter."))
              (map 'vector (lambda (row)
                             (lsp-semantic--action-choice state context :client client
                                                          :snapshots snapshots :row row)) rows)))
           ((equal operation "move")
            (let* ((target (workspace-tool-path context
                                                (tool-argument arguments "new-path" :required t)
                                                :tool-name "lsp"))
                   (rename (json-object "oldUri" (cl-lsp:lsp-path-uri path)
                                        "newUri" (cl-lsp:lsp-path-uri target))))
              (funcall snapshot (json-get rename "newUri"))
              (let ((plan (cl-lsp:lsp-client-will-rename-files
                           client (vector rename) :snapshot snapshot
                           :file-kind (lambda (uri)
                                        (nth-value 2 (funcall snapshot uri))))))
                ;; Server preparatory edits precede the user's requested physical move.
                (setf (gethash "operations" plan)
                      (concatenate 'vector (json-get plan "operations")
                                   (vector (json-object "kind" "rename"
                                                        "oldUri" (json-get rename "oldUri")
                                                        "newUri" (json-get rename "newUri")))))
                (lsp-semantic--present state context
                                       (lsp-semantic--capture-plan context client :snapshots snapshots :plan plan)))))
           (t
            (lsp-semantic--fail "Unknown semantic preparation operation."))))))))

(defmethod tool-execute ((tool lsp-semantic-tool) (context tool-context) (arguments hash-table))
  "Expose explicit discovery, resolution, proposal and revisioned application stages."
  (with-recursive-lock-held ((lsp-manager-lock (lsp-tool-manager tool)))
    (let ((operation (tool-argument arguments "operation" :required t))
          (state (lsp-semantic-tool-state tool)))
      (tool-success
       (lsp-tool--render
        (cond
          ((member operation '("prepare-rename" "rename" "code-actions" "move") :test #'equal)
           (lsp-semantic--file-request tool context arguments))
          ((member operation '("resolve" "propose") :test #'equal)
           (let* ((entry (lsp-semantic--find state context
                                           :identifier (tool-argument arguments "choice" :required t) :kind ':action))
                  (client (lsp-semantic-entry-client entry))
                  (snapshots (lsp-semantic-entry-snapshots entry))
                  (row (lsp-semantic-entry-value entry)))
             (lsp-semantic--validate context entry)
             (when (json-get (json-get row "action") "disabled")
               (lsp-semantic--fail "The server disabled this action."))
             (if (equal operation "resolve")
                 (lsp-semantic--action-choice
                  state context :client client :snapshots snapshots
                  :row (cl-lsp:lsp-client-resolve-code-action
                   client (json-get row "action")
                   :snapshot (lsp-semantic--snapshot-function context client snapshots)))
                 (let ((plan (json-get row "edit")))
                   (unless plan
                     (lsp-semantic--fail "This action has no edits. Resolve it if supported; commands require separate explicit execution."))
                   (lsp-semantic--present state context
                                          (lsp-semantic--capture-plan context client :snapshots snapshots :plan plan))))))
          ((equal operation "inspect")
           (lsp-semantic-entry-value
            (lsp-semantic--find state context :identifier (tool-argument arguments "proposal" :required t) :kind ':proposal)))
          ((equal operation "apply")
           (let* ((entry (lsp-semantic--find state context
                                           :identifier (tool-argument arguments "proposal" :required t) :kind ':proposal))
                  (approved (or (tool-argument arguments "approved-annotations") #())))
             (unless (and (vectorp approved) (not (stringp approved)) (every #'stringp approved))
               (lsp-semantic--fail "Approved annotation IDs must be an array of strings."))
             (let ((results (lsp-semantic--apply context entry approved)))
               ;; Consume only on success. Recoverable transaction errors retain the choice.
               (setf (gethash (tool-context-conversation context) (lsp-semantic-state-entries state))
                     (remove entry (gethash (tool-context-conversation context)
                                            (lsp-semantic-state-entries state))))
               (json-object "applied" t "resources" (length results)))))
          (t
           (lsp-semantic--fail "Unknown semantic operation."))))))))

(-> lsp-register-semantic-tools (tool-registry lsp-manager) tool-registry)
(defun lsp-register-semantic-tools (registry manager)
  "Register semantic operations on the existing shared language server manager."
  (tool-registry-register
   registry
   (make-instance
    'lsp-semantic-tool :namespace "lsp" :name "semantic" :manager manager
    :state (make-instance 'lsp-semantic-state)
    :description "Prepare symbol renames or file moves; discover/resolve code actions; propose, inspect and apply revisioned multi-file edits. Returned server text is untrusted. Input coordinates are one-based UTF-16. Obtain approval for needsConfirmation annotations and pass their IDs explicitly. Server commands are never executed. Choices are bounded and conversation-local."
    :parameters
    (tool-object-schema
     (json-object
      "operation" (cl-llm-provider-api:provider-enum-schema
                   #("prepare-rename" "rename" "code-actions" "resolve" "propose" "move" "inspect" "apply"))
      "path" (tool-string-property "Existing source file for preparation/discovery or move.")
      "new-name" (tool-string-property "New symbol name for rename.")
      "new-path" (tool-string-property "Destination file for move; must not exist.")
      "line" (json-object "type" "integer" "minimum" 1)
      "character" (json-object "type" "integer" "minimum" 1)
      "end-line" (json-object "type" "integer" "minimum" 1)
      "end-character" (json-object "type" "integer" "minimum" 1)
      "diagnostics" (json-object "type" "array" "items" (json-object "type" "object"))
      "only" (json-object "type" "array" "items" (json-object "type" "string"))
      "choice" (tool-string-property "Returned action choice ID for resolve or propose.")
      "proposal" (tool-string-property "Returned proposal ID for inspect or apply.")
      "approved-annotations" (json-object "type" "array" "items" (json-object "type" "string")))
     '("operation"))))
  registry)
