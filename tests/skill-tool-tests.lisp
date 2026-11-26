(in-package #:autolith)

;;;; -- Skill Selection Tool Tests --

(-> skill-tool-tests--write (pathname string string) pathname)
(defun skill-tool-tests--write (root relative-path content)
  "Write CONTENT beneath ROOT at RELATIVE-PATH and return its pathname."
  (test-write-text (merge-pathnames relative-path root) content))

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
    (let* ((body         "SELECTED-SKILL-PROVIDER-BOUNDARY-PROBE")
           (conversation (conversation-create configuration :identifier "skill-provider"))
           (registry     (skill-augment-tool-registry (make-instance 'tool-registry)))
           (namespaces   (tool-registry-provider-schemas registry))
           (context      (make-instance 'tool-context :configuration configuration
                                                     :worker nil :conversation conversation
                                                     :registry       registry))
           (providers    (list (provider-create configuration)
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
           (test-assert
            (not (search body (json-encode
                               (provider-request-object provider conversation #()))))
            "requests without skill.load do not receive selected instructions")
           (let* ((ordinary   (test-request-history
                               provider (provider-request-object provider conversation namespaces)))
                  (compaction (test-request-history
                               provider (provider-request-object provider conversation namespaces
                                                                 :compaction-p t)))
                  (shared     (1- (length compaction))))
             (test-assert (equal (subseq ordinary 0 shared) (subseq compaction 0 shared))
                          "compaction repeats the history ordinary requests send, anchored instructions included"))))
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
        :key  #'context-contribution-identifier
        :test #'string=))

(-> test-skill-source-resource () null)
(defun test-skill-source-resource ()
  "Exercise revisioned creation, validation, atomic publication and native precedence."
  (with-test-configuration (configuration)
    (let* ((registry     (make-default-tool-registry))
           (conversation (conversation-create configuration))
           (context      (make-instance 'tool-context :configuration configuration
                                                     :worker nil :conversation conversation :registry registry))
           (root         (skill-global-root configuration))
           (pathname     (merge-pathnames "editable/SKILL.md" root))
           (uri          "skill-source:editable")
           (initial      (skill-tests--agent-definition "editable" "An editable skill." "First body.")))
      (labels ((read-source ()
                 (test-resource-call registry context :namespace "resource" :name "read"
                                                  :arguments (list "uri" uri)))

               (revision (result)
                 (test-resource-field (tool-result-content result) "Revision: "))

               (edit (base operation)
                 (test-resource-call registry context :namespace "resource" :name "edit"
                                                  :arguments (list "uri" uri "base-revision" base
                                                                   "operations" (vector operation))))

               (replace-line (line content)
                 (json-object "op" "replace-lines" "start-line" line "end-line" line "content" content)))
        (let* ((missing (read-source))
               (base    (revision missing))
               (create  (json-object "op" "replace-empty" "content" initial)))
          (test-assert (not (tool-result-success-p missing)) "observe missing source before creation")
          (test-assert (tool-result-success-p (edit base create)) "create a validated Markdown skill")
          (test-assert (not (tool-result-success-p (edit base create))) "refuse stale creation"))
        (let* ((read (read-source))
               (base (revision read))
               (line (position "First body." (text--split-lines initial) :test #'string=)))
          (test-assert (string= initial (uiop:read-file-string pathname)) "creation writes the supplied source")
          (test-assert (not (tool-result-success-p (edit base (replace-line 1 "invalid frontmatter"))))
                       "invalid prospective definitions are refused")
          (test-assert (string= initial (uiop:read-file-string pathname)) "validation failure preserves source")
          (let ((*workspace-file-resource-publish-function*
                  (lambda (source target)
                    (declare (ignore source))
                    (error 'file-error :pathname target))))
            (test-assert (not (tool-result-success-p (edit base (replace-line (1+ line) "Second body."))))
                         "failed atomic publication is reported"))
          (test-assert (and (string= initial (uiop:read-file-string pathname))
                            (= 1 (length (uiop:directory-files (uiop:pathname-directory-pathname pathname)))))
                       "failed publication preserves the original and removes its temporary file")
          (test-assert (tool-result-success-p (edit base (replace-line (1+ line) "Second body.")))
                       "edit an observed line using the original revision")
          (test-assert (skill-catalog-find (skill-catalog-for-configuration configuration) "editable")
                       "the edited skill is discoverable"))
        (let* ((markdown (revision (read-source)))
               (source   (skill-tests--definition "editable" "Native skill." "Native body."))
               (native   (skill-tool-tests--write root "editable/SKILL.sexp" source))
               (read     (read-source)))
          (test-assert (search "Native body." (tool-result-content read)) "native source takes precedence")
          (test-assert (not (tool-result-success-p (edit markdown (replace-line 1 "stale Markdown"))))
                       "native precedence invalidates a Markdown observation")
          (let ((line (position-if (lambda (text) (search "Native body." text)) (text--split-lines source))))
            (let ((result (edit (revision read)
                                (replace-line (1+ line) (format nil " :instructions ~S)" "New native body.")))))
              (test-assert (tool-result-success-p result)
                           (format nil "Edit and validate the native definition: ~A" (tool-result-content result)))))
          (test-assert (and (search "New native body." (uiop:read-file-string native))
                            (search "Second body." (uiop:read-file-string pathname)))
                       "editing the native source does not overwrite Markdown")))))
  nil)

(-> test-skill-load-tool () null)
(defun test-skill-load-tool ()
  "Test exact, ephemeral Skill selection through skill.load."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (project            (merge-pathnames "project/" root))
         (skill-root         (merge-pathnames ".autolith/skills/" project))
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
         (tool               (tool-registry-find registry "skill" "load"))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker        nil
                          :conversation  conversation
                          :registry      registry)))
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
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-skill-load-presentation () null)
(defun test-skill-load-presentation ()
  "Test compact transcript markers and malformed Skill result rejection."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "skill-load-presentation"))
         (registry      (skill-augment-tool-registry
                         (make-instance 'tool-registry)))
         (terminal      (make-instance 'recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :tool-registry registry
                          :ui            ui))
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
            (list :tool      "skill.load"
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
             (terminal-ui-refresh-size ui (lambda (target)
                                             (declare (ignore target))
                                             (cons 25 79)))
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
            (list :tool      "skill.load"
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
                     (list :tool      "skill.load"
                           :success-p nil
                           :details
                           '(:kind :skill-load
                             :name "failed"
                             :newly-selected-p t))
                     (list :tool      "skill.load"
                           :success-p t
                           :details
                           '(:kind :skill-load
                             :name "missing-state"))
                     (list :tool      "skill.load"
                           :success-p t
                           :details
                           '(:kind :other
                             :name "wrong-kind"
                             :newly-selected-p t))
                     (list :tool      "skill.load"
                           :success-p t
                           :details
                           '(:kind :skill-load
                             :name "invalid-state"
                             :newly-selected-p :yes))
                     (list :tool      "skill.load"
                           :success-p t
                           :details   "not-a-property-list")))
             (recording-terminal-reset terminal)
             (funcall send-status ':tool-call-completed details)
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (and (null (search "◆ " output))
                     (= (test-substring-count "skill.load" output) 1))
                "failed and malformed Skill results use one ordinary result"))))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)


;;;; -- Cache-Prefix Projection --

(-> test-skill-cache-prefix () null)
(defun test-skill-cache-prefix ()
  "Exercise stable skill placement, live edits, expiry, and metadata replay."
  (with-test-configuration (configuration)
    (let* ((*context-contributors* nil)
           (*context-resolver*     (cl-llm-provider-api:make-context-resolver))
           (conversation           (conversation-create configuration :identifier "skill-cache"))
           (registry               (skill-augment-tool-registry (make-instance 'tool-registry)))
           (schemas                (tool-registry-provider-schemas registry))
           (context                (make-instance 'tool-context :configuration configuration
                                                              :worker nil :conversation conversation
                                                              :registry        registry))
           (provider               (provider-create configuration))
           (body                   (concatenate 'string "ALPHA-CACHE-BODY-" (make-string 16000 :initial-element #\a)))
           (state                  "Mutable state one."))
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
                 (let* ((tool        (tool-registry-find registry "skill" "load"))
                        (persistence (tool-conversation-persistence tool))
                        (call-id     (format nil "select-~A" name)))
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
                 "Return REQUEST's input without the deliberately changing mutable-state message."
                 (remove-if (lambda (item) (search "Mutable state" (json-encode item)))
                            (json-get request "input")))

               (prefix-p (earlier later)
                 "Return true when EARLIER's stable input is a prefix of LATER's."
                 (let ((prefix (stable-input earlier))
                       (input  (stable-input later)))
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
                 (let ((resumed (conversation-load (conversation-pathname conversation))))
                   (test-assert
                    (prefix-p second
                              (let ((*context-request-contributions*
                                      (list (make-context-contribution
                                             :identifier "mutable" :instruction state
                                             :class      ':mandatory))))
                                (provider-request-object provider resumed schemas)))
                    "a resumed conversation keeps selected skills at their boundaries"))
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
               (let ((ordinary   (json-get (request) "input"))
                     (compaction (json-get (provider-request-object provider conversation schemas
                                                                    :compaction-p t)
                                           "input")))
                 (test-assert (equal (map 'list #'json-encode
                                          (subseq ordinary 0 (1- (length compaction))))
                                     (map 'list #'json-encode
                                          (subseq compaction 0 (1- (length compaction)))))
                              "a compaction request repeats anchored skill context where ordinary requests placed it"))
               (let ((summary (json-object "role" "user" "content" "Compacted work.")))
                 (setf (conversation-input-items conversation) (list summary))
                 (incf (conversation-log-generation conversation))
                 (let ((input (json-get (request) "input")))
                   (test-assert
                    (and (search "Compacted work." (json-encode (aref input 0)))
                         (search "UPDATED-ALPHA-BODY" (json-encode input)))
                    "removed history boundaries relocate active context after compaction")))))))
        (let* ((next     (json-encode (request)))
               (loaded   (conversation-load (conversation-pathname conversation)))
               (replayed (json-encode (coerce (conversation-input-items loaded) 'vector))))
          (test-assert (and (not (search "UPDATED-ALPHA-BODY" next))
                            (not (search "BETA-CACHE-BODY" next)))
                       "selection expires at the logical-turn boundary")
          (test-assert (and (search "select-alpha" replayed) (search "select-beta" replayed)
                            (not (search body replayed))
                            (not (search "UPDATED-ALPHA-BODY" replayed)))
                       "selection metadata replays without skill bodies")
          (let ((anchors (uiop:read-file-string
                          (conversation-context-anchors-pathname
                           (conversation-pathname conversation)))))
            (test-assert (and (not (search "ALPHA-CACHE-BODY" anchors))
                              (not (search "UPDATED-ALPHA-BODY" anchors))
                              (not (search "Cache projection fixture" anchors)))
                         "durable anchors hold digests and positions, never context"))))))
  nil)

(-> test-skill-description-resource () null)
(defun test-skill-description-resource ()
  "Read a shortened catalog description whole through skill:NAME, selecting nothing."
  (with-test-configuration (configuration)
    (let* ((description (format nil "~A END-OF-DESCRIPTION"
                                (make-string 400 :initial-element #\d)))
           (registry    (make-default-tool-registry :configuration configuration))
           (context     (make-instance 'tool-context
                                       :configuration configuration
                                       :worker        nil
                                       :conversation  (conversation-create
                                                       configuration :identifier "skill-resource")
                                       :registry      registry)))
      (unwind-protect
           (flet ((read-uri (uri)
                    (tool-registry-execute-call
                     registry
                     (json-object "namespace" "resource" "name" "read"
                                  "arguments" (json-encode (json-object "uri" uri)))
                     context)))
             (skill-tool-tests--write
              (skill-global-root configuration) "long/SKILL.sexp"
              (skill-tests--definition "long" description "LONG-SKILL-BODY"))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Pick a skill.")
              (lambda ()
                (let ((catalog (skill-catalog-render
                                (skill-catalog-for-configuration configuration)))
                      (result  (read-uri "skill:long")))
                  (test-assert
                     (and (search "`resource.read` of `skill:NAME`" catalog)
                        (search "skill:NAME"
                                (tool-description
                                 (tool-registry-find registry "resource" "read"))))
                     "the catalog and tool direct readers to the skill resource")
                  (test-assert
                   (and (tool-result-success-p result)
                        (search description (tool-result-content result))
                        (not (search "LONG-SKILL-BODY" (tool-result-content result))))
                   "skill:NAME returns the whole description and no instructions")
                  (test-assert (null (skill-logical-turn-selection-names))
                               "reading a description selects nothing")
                  (test-assert (not (tool-result-success-p (read-uri "skill:missing")))
                               "an unknown skill name fails")))))
        (tool-registry-close-runtime-state registry))))
  nil)
