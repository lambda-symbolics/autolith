(in-package #:autolith)

;;;; -- Skill Selection Tool Tests --

(-> skill-tool-tests--write (pathname string string) pathname)
(defun skill-tool-tests--write (root relative-path content)
  "Write CONTENT beneath ROOT at RELATIVE-PATH and return its pathname."
  (let ((pathname (merge-pathnames relative-path root)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction ':output
                            :if-does-not-exist ':create
                            :if-exists ':supersede
                            :external-format ':utf-8)
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

(-> test-skill-provider-context () null)
(defun test-skill-provider-context ()
  "Exercise selection and ephemeral injection through every provider projection."
  (with-test-configuration (configuration)
    (let* ((body "SELECTED-SKILL-PROVIDER-BOUNDARY-PROBE")
           (conversation (conversation-create configuration :identifier "skill-provider"))
           (registry (skill-edit-augment-tool-registry
                      (skill-augment-tool-registry (make-instance 'tool-registry))))
           (namespaces (tool-registry-provider-schemas registry))
           (hidden (tool-registry-provider-schemas registry :canonical-names '("skill.edit")))
           (context (make-instance 'tool-context :configuration configuration
                                                :worker nil :conversation conversation
                                                :registry registry))
           (providers (list (provider-create configuration)
                            (grok-provider-create configuration)
                            (openai-compatible-provider-create
                             configuration :name "skill-test" :family ':skill-test)
                            (anthropic-provider-create configuration)
                            (gemini-code-assist-provider-create configuration))))
      (skill-tool-tests--write
       (skill-global-root configuration) "probe/SKILL.sexp"
       (skill-tests--definition "probe" "Probe request-local injection." body))
      (conversation-append-user-message conversation "Use the probe skill.")
      (dolist (provider providers)
        (call-with-skill-logical-turn
         (user-message-input-create :text "Select probe.")
         (lambda ()
           (test-assert
            (not (search body (json-encode
                              (provider-request-object provider conversation namespaces))))
            "an unselected skill contributes only catalog metadata")
           (let ((result (skill-tool-tests--call registry context "probe")))
             (test-assert (and (tool-result-success-p result)
                               (not (search body (tool-result-content result))))
                          "skill.load selects without returning instructions"))
           (dotimes (attempt 2)
             (multiple-value-bind (request delivery)
                 (provider-request-object provider conversation namespaces)
               (test-assert
                (and (search body (json-encode request))
                     (skill-tool-tests--contribution
                      (context-delivery-contributions delivery) "skill-selected-probe"))
                (format nil "~A injects the selected body on subsequent requests"
                        (class-name (class-of provider))))))
           (dolist (schemas (list #() hidden))
             (test-assert
              (not (search body (json-encode
                                (provider-request-object provider conversation schemas))))
              "requests without skill.load do not receive selected instructions"))
           (test-assert
            (not (search body (json-encode
                              (provider-request-object provider conversation namespaces
                                                       :compaction-p t))))
            "compaction excludes selected instructions")))
        (test-assert
         (not (search body (json-encode
                           (provider-request-object provider conversation namespaces))))
         "selection ends with the logical turn"))
      (test-assert
       (and (= 1 (length (conversation-input-items conversation)))
            (not (search body (json-encode (conversation-input-items conversation)))))
       "provider context never persists the selected body")))
  nil)

(-> skill-tool-tests--contribution
    (list string)
    (option context-contribution))
(defun skill-tool-tests--contribution (contributions identifier)
  "Return the contribution named IDENTIFIER from CONTRIBUTIONS."
  (find identifier
        contributions
        :key #'context-contribution-identifier
        :test #'string=))

(-> test-skill-edit-tool () null)
(defun test-skill-edit-tool ()
  "Exercise global skill creation, replacement, validation, and failed publication."
  (with-test-configuration (configuration)
    (let* ((registry (skill-edit-augment-tool-registry (make-instance 'tool-registry)))
           (authoring-configuration
             (configuration-copy
              configuration
              :cache-root (uiop:ensure-directory-pathname
                           (skill-tool-tests--write (config :cache-root configuration)
                                                    "unavailable" "Not a directory."))))
           (context (make-instance 'tool-context :configuration authoring-configuration
                                                :worker nil :registry registry))
           (root (skill-global-root configuration))
           (pathname (merge-pathnames "editable/SKILL.md" root))
           (initial (skill-tests--agent-definition "editable" "An editable skill." "First body."))
           (replacement (skill-tests--agent-definition "editable" "Updated skill." "Second body.")))
      (labels ((edit (name content)
                 (tool-registry-execute-call
                  registry
                  (json-object "namespace" "skill" "name" "edit"
                               "arguments" (json-encode (json-object "name" name "content" content)))
                  context)))
        (test-assert (tool-result-success-p (edit "editable" initial))
                     "skill.edit creates a global skill without writable cache storage")
        (test-assert (string= initial (uiop:read-file-string pathname))
                     "creation writes the supplied source")
        (test-assert (skill-catalog-find
                      (skill-catalog-discover (list root)
                                              :cache-root (config :cache-root configuration))
                      "editable")
                     "the created skill is discoverable")
        (test-assert (tool-result-success-p (edit "editable" replacement))
                     "skill.edit replaces an existing Markdown skill")
        (dolist (entry (list (list "../editable" initial)
                             (list "Bad-Name" initial)
                             (list "editable" "Missing frontmatter")
                             (list "editable" (skill-tests--agent-definition
                                               "other" "Wrong name." "Body."))
                             (list "editable" (skill-tests--agent-definition
                                               "editable" "Oversized source."
                                               (make-string cl-skills:*skill-file-character-limit*
                                                            :initial-element #\x)))
                             (list "editable" (make-string (1+ *skill-edit-maximum-content-characters*)
                                                           :initial-element #\x))))
          (test-assert (not (tool-result-success-p (apply #'edit entry)))
                       "invalid skill edits fail"))
        (test-assert (string= replacement (uiop:read-file-string pathname))
                     "validation failures preserve the prior skill")
        (let ((rename #'uiop:rename-file-overwriting-target))
          (test-call-with-function-replacements
           (list (list 'uiop:rename-file-overwriting-target
                       (lambda (source target)
                         (if (equal target pathname)
                             (error 'file-error :pathname target)
                             (funcall rename source target)))))
           (lambda ()
             (test-assert (not (tool-result-success-p (edit "editable" initial)))
                          "publication failure is reported"))))
        (test-assert (and (string= replacement (uiop:read-file-string pathname))
                          (= 1 (length (uiop:directory-files
                                        (uiop:pathname-directory-pathname pathname)))))
                     "failed publication preserves the original and removes its temporary file")
        (skill-tool-tests--write root "editable/SKILL.sexp"
                                 (skill-tests--definition "editable" "Native skill." "Native body."))
        (test-assert (not (tool-result-success-p (edit "editable" initial)))
                     "a native skill cannot silently shadow an edited Markdown skill"))))
  nil)

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
             (configuration-copy
              base-configuration :working-directory
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
             (and (eq (tool-conversation-persistence tool) ':durable)
                  (tool-provider-round-trip-barrier-p tool))
             "skill.load retains selection metadata and requires a provider barrier")
           (let ((schema (tool-provider-schema tool)))
             (test-assert
              (and (string= (json-get schema "name") "load")
                   (equal (coerce
                           (json-get (json-get schema "parameters")
                                     "required")
                           'list)
                          '("name"))
                   (json-false-p (gethash "additionalProperties" (json-get schema "parameters"))))
              "skill.load exposes one required exact-name argument"))
           (let ((outside-turn
                   (skill-tool-tests--call registry context "alpha")))
             (test-assert
              (and (not (tool-result-success-p outside-turn))
                   (eq (tool-result-error-code outside-turn) ':inactive-turn))
              "skill.load rejects selection that cannot survive a logical turn"))
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
                 (equal (skill-logical-turn-selection-names) '("alpha"))
                 "skill.load accumulates selection in logical-turn state")
                (test-assert
                 (and (< (length (tool-result-content result)) 256)
                      (not (search secret-body
                                   (tool-result-content result)))
                      (equal (tool-result-details result)
                             '(:kind :skill-load
                               :name "alpha"
                               :newly-selected-p t))
                      (null (tool-result-image-attachments result)))
                 "the request-local result contains bounded presentation metadata")
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
                        (equal (tool-result-details duplicate)
                               '(:kind :skill-load
                                 :name "alpha"
                                 :newly-selected-p nil))
                         (equal (skill-logical-turn-selection-names)
                                '("alpha")))
                     "repeated selection is idempotent")))))
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
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-skill-load-presentation () null)
(defun test-skill-load-presentation ()
  "Test compact transcript markers and malformed Skill result rejection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "skill-load-presentation"))
         (registry (skill-augment-tool-registry
                    (make-instance 'tool-registry)))
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :tool-registry registry
                          :ui ui))
         (observer
           (application-agent-observer
            application
            :user-message-input (user-message-input-create :text "pending")))
         (send-status
           (callback-agent-observer-status-callback observer)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (recording-terminal-reset terminal)
           (funcall
            send-status
            ':tool-call-completed
            (list :tool "skill.load"
                  :success-p t
                  :details
                  '(:kind :skill-load
                    :name "code-review"
                    :newly-selected-p t)))
           (let ((identifier
                   (list ':presentation
                         (application-presentation-counter application))))
             (test-assert
              (search "loaded skill: code-review"
                      (recording-terminal-output terminal))
              "a successful compact Skill selection finalizes a visible marker")
             (test-assert
              (gethash identifier (terminal-ui-finalized-identifiers ui))
              "the Skill marker is retained as finalized scrollback")
             (recording-terminal-reset terminal)
             (terminal-ui-refresh-size ui (lambda () (cons 25 79)))
             (test-assert
              (and (gethash identifier
                            (terminal-ui-finalized-identifiers ui))
                   (null (search "◆ loaded skill"
                                 (recording-terminal-output terminal))))
              "a live repaint preserves finalized scrollback without replaying it"))
           (recording-terminal-reset terminal)
           (funcall
            send-status
            ':tool-call-completed
            (list :tool "skill.load"
                  :success-p t
                  :details
                  '(:kind :skill-load
                    :name "code-review"
                    :newly-selected-p nil)))
           (test-assert
            (search "◆ skill already loaded: code-review"
                    (recording-terminal-output terminal))
            "a repeated Skill selection remains visibly distinguishable")
           (dolist (details
                    (list
                     (list :tool "skill.load"
                           :success-p nil
                           :details
                           '(:kind :skill-load
                             :name "failed"
                             :newly-selected-p t))
                     (list :tool "skill.load"
                           :success-p t
                           :details
                           '(:kind :skill-load
                             :name "missing-state"))
                     (list :tool "skill.load"
                           :success-p t
                           :details
                           '(:kind :other
                             :name "wrong-kind"
                             :newly-selected-p t))
                     (list :tool "skill.load"
                           :success-p t
                           :details
                           '(:kind :skill-load
                             :name "invalid-state"
                             :newly-selected-p :yes))
                     (list :tool "skill.load"
                           :success-p t
                           :details "not-a-property-list")))
             (recording-terminal-reset terminal)
             (funcall send-status ':tool-call-completed details)
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (and (null (search "◆ " output))
                     (= (terminal-tests--substring-count "skill.load" output) 1))
                "failed and malformed Skill results use one ordinary result")))
           (setf (application-compact-view-p application) nil)
           (recording-terminal-reset terminal)
           (funcall
            send-status
            ':tool-call-completed
            (list :tool "skill.load"
                  :success-p t
                  :output "Selected skill expanded-view for this logical turn."
                  :details
                  '(:kind :skill-load
                    :name "expanded-view"
                    :newly-selected-p t)))
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (null (search "◆ loaded skill" output))
                   (= (terminal-tests--substring-count "✓ skill.load" output) 1)
                   (search "Selected skill expanded-view" output))
              "expanded tool view presents its ordinary result exactly once")))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)


;;;; -- Cache-Prefix Projection --

(-> test-skill-cache-prefix () null)
(defun test-skill-cache-prefix ()
  "Exercise stable skill placement, live edits, expiry, and metadata replay."
  (with-test-configuration (configuration)
    (let* ((*context-contributors* nil)
           (*context-resolver* (cl-llm-provider-api:make-context-resolver))
           (conversation (conversation-create configuration :identifier "skill-cache"))
           (registry (skill-augment-tool-registry (make-instance 'tool-registry)))
           (schemas (tool-registry-provider-schemas registry))
           (context (make-instance 'tool-context :configuration configuration
                                               :worker nil :conversation conversation
                                               :registry registry))
           (provider (provider-create configuration))
           (body (concatenate 'string "ALPHA-CACHE-BODY-" (make-string 16000 :initial-element #\a)))
           (state "Mutable state one."))
      (register-context-contributor "skills" 'skill-context-contributor :source ':built-in)
      (dolist (name '("alpha" "beta"))
        (skill-tool-tests--write
         (skill-global-root configuration) (format nil "~A/SKILL.sexp" name)
         (skill-tests--definition name "Cache projection fixture."
                                  (if (string= name "alpha") body "BETA-CACHE-BODY"))))
      (labels ((request (&optional (namespaces schemas))
                 (let ((*context-request-contributions*
                         (list (make-context-contribution
                                :identifier "mutable" :instruction state :class ':mandatory))))
                   (provider-request-object provider conversation namespaces)))

               (select (name)
                 (let* ((tool (tool-registry-find registry "skill" "load"))
                        (persistence (tool-conversation-persistence tool))
                        (call-id (format nil "select-~A" name)))
                   (conversation-append-provider-item
                    conversation
                    (json-object "type" "function_call" "namespace" "skill" "name" "load"
                                 "call_id" call-id
                                 "arguments" (json-encode (json-object "name" name)))
                    :persistence persistence)
                   (let ((result (skill-tool-tests--call registry context name)))
                     (test-assert (tool-result-success-p result) "skill selection succeeds")
                     (conversation-append-tool-result
                      conversation call-id :tool-name "skill.load"
                      :output (tool-result-content result) :success-p t :persistence persistence))))

               (stable-input (request)
                 (let ((input (json-get request "input")))
                   (subseq input 0 (1- (length input)))))

               (prefix-p (earlier later)
                 (let ((prefix (stable-input earlier))
                       (input (json-get later "input")))
                   (and (<= (length prefix) (length input))
                        (string= (json-encode prefix)
                                 (json-encode (subseq input 0 (length prefix))))))))
        (conversation-append-user-message conversation "Initial work.")
        (let ((initial (request)))
          (call-with-skill-logical-turn
           (user-message-input-create :text "Load skills.")
           (lambda ()
             (select "alpha")
             (let ((first (request)))
               (test-assert (prefix-p initial first)
                            "loading a skill preserves the earlier catalog prefix")
               (test-assert (search body (json-encode (stable-input first)))
                            "the reusable input includes the complete large skill body")
               (conversation-clear-ephemeral-input-items conversation)
               (conversation-append-user-message conversation "Continue work.")
               (setf state "Mutable state two.")
               (let ((second (request)))
                 (test-assert (prefix-p first second)
                              "growing history and mutable context do not move selected skills")
                 (test-assert
                  (and (string= (json-get first "instructions") (json-get second "instructions"))
                       (string= (json-encode (json-get first "tools"))
                                (json-encode (json-get second "tools"))))
                  "equivalent requests retain stable instruction and tool definitions")
                 (select "beta")
                 (test-assert (prefix-p second (request))
                              "adding a second skill preserves previously projected blocks"))
               (skill-tool-tests--write
                (skill-global-root configuration) "alpha/SKILL.sexp"
                (skill-tests--definition "alpha" "Cache projection fixture." "UPDATED-ALPHA-BODY"))
               (let ((edited (json-encode (request))))
                 (test-assert (and (search "UPDATED-ALPHA-BODY" edited) (not (search body edited)))
                              "anchored skill contents are reread after edits"))
               (test-assert (not (search "UPDATED-ALPHA-BODY" (json-encode (request #()))))
                            "tool restrictions remove skill instructions")
               (request)
               (let ((compaction (provider-request-object provider conversation schemas :compaction-p t)))
                 (test-assert (not (search "UPDATED-ALPHA-BODY" (json-encode compaction)))
                              "compaction side requests omit skill context"))
               (let ((summary (json-object "role" "user" "content" "Compacted work.")))
                 (setf (conversation-input-items conversation) (list summary))
                 (incf (conversation-log-generation conversation))
                 (let ((input (json-get (request) "input")))
                   (test-assert
                    (and (search "Compacted work." (json-encode (aref input 0)))
                         (search "UPDATED-ALPHA-BODY" (json-encode input)))
                    "removed history boundaries relocate active context after compaction")))))))
        (let* ((next (json-encode (request)))
               (loaded (conversation-load (conversation-pathname conversation)))
               (replayed (json-encode (coerce (conversation-input-items loaded) 'vector))))
          (test-assert (and (not (search "UPDATED-ALPHA-BODY" next))
                            (not (search "BETA-CACHE-BODY" next)))
                       "selection expires at the logical-turn boundary")
          (test-assert (and (search "select-alpha" replayed) (search "select-beta" replayed)
                            (not (search body replayed))
                            (not (search "UPDATED-ALPHA-BODY" replayed))
                            (null (conversation-context-anchors loaded)))
                       "selection metadata replays without skill bodies or process-local anchors")))))
  nil)
