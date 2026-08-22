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

(-> skill-tool-tests--contribution
    (list string)
    (option context-contribution))
(defun skill-tool-tests--contribution (contributions identifier)
  "Return the contribution named IDENTIFIER from CONTRIBUTIONS."
  (find identifier
        contributions
        :key #'context-contribution-identifier
        :test #'string=))

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
             (configuration-with-working-directory
              base-configuration
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
            (and (eq (tool-conversation-persistence tool) ':next-response)
                 (tool-provider-round-trip-barrier-p tool))
            "skill.load declares request-local persistence and a provider barrier")
           (let ((schema (tool-provider-schema tool)))
             (test-assert
              (and (string= (json-get schema "name") "load")
                   (equal (coerce
                           (json-get (json-get schema "parameters")
                                     "required")
                           'list)
                          '("name"))
                   (eq (json-get (json-get schema "parameters")
                                 "additionalProperties")
                       false))
              "skill.load exposes one required exact-name argument"))
           (let ((outside-turn
                   (skill-tool-tests--call registry context "alpha")))
             (test-assert
              (and (not (tool-result-success-p outside-turn))
                   (search "only while an agent turn is active"
                           (tool-result-content outside-turn)))
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
                (equal *skill-logical-turn-selection-names* '("alpha"))
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
                        (equal *skill-logical-turn-selection-names*
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
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-skill-load-presentation () null)
(defun test-skill-load-presentation ()
  "Test finalized transcript markers for request-local Skill selections."
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
         (observer (application-agent-observer application))
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
           (test-assert
            (search "◆ loaded skill: code-review"
                    (recording-terminal-output terminal))
            "a successful Skill selection finalizes a visible transcript marker")
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
            "a repeated Skill selection remains visibly distinguishable"))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)
