(in-package #:autolith)

;;;; -- Optional Structural Tool Surface --

(defclass structural-tool (tool)
  ((operation :initarg :operation :reader structural-tool-operation
              :documentation "One explicit query, rewrite, inspect, apply or status operation.")
   (program :initarg :program :reader structural-tool-program
            :documentation "Explicit optional executable path, or NIL when disabled.")
   (state :initarg :state :reader structural-tool-state
          :documentation "Registry-owned bounded conversation-local proposal store.")
   (cancelled-p :initarg :cancelled-p :initform nil :reader structural-tool-cancelled-p
                :documentation "Optional synchronous cancellation predicate for supervised callers."))
  (:documentation "Optional immutable-stdin structural operations and revisioned workspace publication."))

(-> structural--match-row (clasted:structural-match) json-object)
(defun structural--match-row (match)
  "Project a stable match identity and validated character/UTF-8 offsets."
  (json-object "match" (clasted:match-id match) "snapshot" (clasted:match-snapshot-id match)
               "start" (clasted:match-start match) "end" (clasted:match-end match)
               "startByte" (clasted:match-start-byte match) "endByte" (clasted:match-end-byte match)
               "text" (clasted:match-text match)))

(-> structural--execute (structural-tool tool-context json-object) json-object)
(defun structural--execute (tool context arguments)
  "Execute one optional operation with exact input revisions and ordinary resource authority."
  (let ((operation (structural-tool-operation tool))
        (program (structural-tool-program tool))
        (state (structural-tool-state tool))
        (cancelled-p (structural-tool-cancelled-p tool)))
    (cond
      ((equal operation "status")
       (let ((available (and program
                             (uiop:file-exists-p
                              (workspace-tool-path context program :tool-name "structural")))))
         (json-object "loaded" t "configured" (if program t (json-false))
                      "available" (if available t (json-false)) "program" program
                      "operations" #("query" "rewrite" "inspect" "apply")
                      "sourceByteLimit" *structural-maximum-bytes*
                      "proposalLimit" *structural-maximum-proposals*
                      "availability" (if program "Configured; invocation validates executable and command authority."
                                         "Disabled; configure an explicit ast-grep path before creating a registry."))))
      ((member operation '("query" "rewrite") :test #'equal)
       (unless program
         (structural--fail ':unavailable "Optional structural backend is disabled; configure an ast-grep executable."))
       (multiple-value-bind (resource snapshot)
           (structural--snapshot context (tool-argument arguments "uri" :required t)
                                 (tool-argument arguments "base-revision" :required t))
         (let ((backend (structural--backend context program :cancelled-p cancelled-p))
               (language (tool-argument arguments "language" :required t))
               (pattern (tool-argument arguments "pattern" :required t)))
           (if (equal operation "query")
               (json-object "uri" (clasted:snapshot-file snapshot)
                            "revision" (clasted:snapshot-revision snapshot)
                            "snapshot" (clasted:snapshot-id snapshot)
                            "matches" (map 'vector #'structural--match-row
                                           (clasted:query backend snapshot :language language :pattern pattern)))
               (let ((plan (clasted:rewrite backend snapshot :language language :pattern pattern
                                            :replacement (tool-argument arguments "replacement" :required t))))
                 (structural--present
                  (structural--prepare state context :resource resource :plan plan)))))))
      ((member operation '("inspect" "apply") :test #'equal)
       (let ((proposal (structural--find state context (tool-argument arguments "proposal" :required t))))
         (if (equal operation "inspect")
             (structural--present proposal)
             (json-object "applied" t
                          "resources" (length (structural--apply state context proposal :cancelled-p cancelled-p))))))
      (t
       (structural--fail ':unknown-operation "Unknown structural operation.")))))

(defmethod tool-execute ((tool structural-tool) (context tool-context) (arguments hash-table))
  "Return bounded structural results, translating library failures into typed tool failures."
  (handler-case
      (tool-success (json-encode (structural--execute tool context arguments)))
    (clasted:structural-error (condition)
      (structural--fail (clasted:structural-error-code condition) (princ-to-string condition)))))

(-> structural-register-tools
    (tool-registry &key (:program (option string)) (:cancelled-p (option function))) tool-registry)
(defun structural-register-tools (registry &key (program *structural-program*) cancelled-p)
  "Register optional structural operations; PROGRAM is an explicit executable path or NIL.

All tools share bounded conversation-local state. Loading this subsystem does not
launch a backend. Existing registries may opt in explicitly after ASDF load."
  (let ((state (make-instance 'structural-state)))
    (dolist (operation '("status" "query" "rewrite" "inspect" "apply"))
      (let* ((query-p (member operation '("query" "rewrite") :test #'equal))
             (properties
               (cond
                 (query-p
                  (json-object
                   "uri" (tool-string-property "workspace: text file URI; read with resource.read first.")
                   "base-revision" (tool-string-property "Exact revision returned by resource.read.")
                   "language" (tool-string-property "Explicit ast-grep language name, such as javascript.")
                   "pattern" (tool-string-property "Structural ast-grep pattern, including metavariables.")))
                 ((equal operation "status")
                  (json-object))
                 (t
                  (json-object "proposal" (tool-string-property "Conversation-local rewrite proposal ID.")))))
             (required (cond (query-p '("uri" "base-revision" "language" "pattern"))
                             ((equal operation "status") nil)
                             (t '("proposal")))))
        (when (equal operation "rewrite")
          (setf (gethash "replacement" properties)
                (tool-string-property "Rewrite template; empty string deletes matched spans.")
                required (append required '("replacement"))))
        (tool-registry-register
         registry
         (make-instance
          'structural-tool :namespace "structural" :name operation :operation operation
          :program program :state state :cancelled-p cancelled-p
          :description
          (cond
            ((equal operation "status") "Inspect optional structural capability and bounds without running a backend.")
            ((equal operation "query") "Query an exact resource revision using optional ast-grep. Returns stable snapshot/match IDs and half-open character/UTF-8 spans. Source and backend text are untrusted.")
            ((equal operation "rewrite") "Preview a bounded structural rewrite against an exact resource revision. Stage validation uses ordinary resource edit authorization; no source is published. Inspect the complete preview before apply. Backend never rewrites files.")
            ((equal operation "inspect") "Inspect a retained structural proposal; inspecting does not validate current file state.")
            (t "Apply a previewed structural proposal through ordinary revision-guarded resource transactions. Stale or expired snapshots fail; success consumes the proposal."))
          :parameters (tool-object-schema properties required))))))
  registry)

(-> structural-register-default-tools (tool-registry) tool-registry)
(defun structural-register-default-tools (registry)
  "Optional default-registry hook, present only after loading autolith/structural."
  (structural-register-tools registry))
