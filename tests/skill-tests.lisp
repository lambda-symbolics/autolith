(in-package #:autolith)

;;;; -- Skill Test Support --

(-> skill-tests--write (pathname string string) pathname)
(defun skill-tests--write (root relative content)
  "Write CONTENT beneath ROOT at RELATIVE and return the resulting pathname."
  (test-write-text (merge-pathnames relative root) content))

(-> skill-tests--definition
    (string string string &key (:version t))
    string)
(defun skill-tests--definition
    (name description instructions &key (version 1))
  "Return one native skill definition string."
  (let ((*print-pretty*   nil)
        (*print-circle*   nil)
        (*print-readably* nil))
    (format nil
            "(:autolith-skill~% :version ~S~% :name ~S~% :description ~S~% :instructions ~S)~%"
            version
            name
            description
            instructions)))

(-> skill-tests--agent-definition (string string string) string)
(defun skill-tests--agent-definition (name description instructions)
  "Return one standard Agent Skill definition string."
  (format nil
          "---~%name: ~S~%description: ~S~%---~%~A"
          name
          description
          instructions))

(-> skill-tests--contribution (list string) (option context-contribution))
(defun skill-tests--contribution (contributions identifier)
  "Return the contribution named IDENTIFIER from CONTRIBUTIONS."
  (find identifier
        contributions
        :key  #'context-contribution-identifier
        :test #'string=))

(-> skill-tests--contribution-identifiers (list) list)
(defun skill-tests--contribution-identifiers (contributions)
  "Return stable identifiers from request-local skill CONTRIBUTIONS."
  (mapcar #'context-contribution-identifier contributions))

(-> skill-tests--delete-root (pathname) null)
(defun skill-tests--delete-root (root)
  "Delete temporary test ROOT when it exists."
  (when (probe-file root)
    (platform-delete-directory-tree *platform* root
                                    :validate          t
                                    :if-does-not-exist ':ignore))
  nil)

(-> skill-tests--tool-namespaces (&key (:load-p boolean)) vector)
(defun skill-tests--tool-namespaces (&key (load-p t))
  "Return a provider namespace vector with optional skill.load visibility."
  (vector
   (json-object
    "type" "namespace"
    "name" "skill"
    "tools"
    (vector
     (json-object
      "name" (if load-p "load" "unavailable"))))))


;;;; -- Root and Catalog Tests --

(-> skill-tests--roots-and-rendering () null)
(defun skill-tests--roots-and-rendering ()
  "Test effective roots, root precedence, and bounded catalog rendering."
  (let* ((site-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-skill-site-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (base-configuration
           (progn
             (ensure-directories-exist site-root)
             (setf site-root
                   (uiop:ensure-directory-pathname
                    (truename site-root)))
             (test-configuration :site-config-root site-root)))
         (root              (test-configuration-root base-configuration))
         (project           (merge-pathnames "project/" root))
         (working-directory (merge-pathnames "src/module/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (ensure-directories-exist
              (merge-pathnames "directory-marker" working-directory))
             (configuration-copy
              base-configuration :working-directory
              working-directory)))
         (project-skills    (merge-pathnames ".autolith/skills/" project))
         (user-skills
           (merge-pathnames
            "skills/"
            (config :config-root configuration)))
         (site-skills       (merge-pathnames "skills/" site-root)))
    (unwind-protect
         (progn
           (let ((roots (skill-roots configuration)))
             (test-assert
              (= (length roots) 5)
              "skill discovery has project, user, shared, site, and bundled roots")
             (test-assert
              (equal (first roots) project-skills)
              "only the effective Git root supplies project-local skills")
             (test-assert
              (equal (second roots) user-skills)
              "the XDG Autolith skill root follows the project root")
             (test-assert
              (equal (third roots)
                     (merge-pathnames ".agents/skills/" (user-homedir-pathname)))
              "the shared ~/.agents/skills root follows the Autolith user root")
             (test-assert
              (equal
               (fourth roots)
               site-skills)
              "the site skill root follows the shared user root")
             (test-assert
              (equal
               (fifth roots)
               (merge-pathnames
                "skills/"
                (config :source-root configuration)))
              "the optional bundled root has lowest precedence"))
           (skill-tests--write
            project-skills
            "winner/SKILL.sexp"
            (skill-tests--definition
             "winner"
             "Project definition."
             "Project instructions."))
           (skill-tests--write
            user-skills
            "winner/SKILL.sexp"
            (skill-tests--definition
             "winner"
             "User definition."
             "User instructions."))
           (skill-tests--write
            user-skills
            "standard/SKILL.md"
            (skill-tests--agent-definition
             "standard"
             "Standard Skill integration."
             "Standard instructions."))
           (let* ((catalog
                    (skill-catalog-for-configuration configuration))
                  (winner   (skill-catalog-find catalog "winner"))
                  (standard (skill-catalog-find catalog "standard")))
             (test-assert
              (equal
               (skill-metadata-pathname winner)
               (truename
                (merge-pathnames "winner/SKILL.sexp" project-skills)))
              "project-local skills take precedence over user skills")
             (test-assert
              (and (eq (skill-metadata-source-format standard) ':agent-skill)
                   (equal (skill-metadata-cache-root standard)
                          (config :cache-root configuration))
                   (string= (skill-metadata-read standard)
                            "Standard instructions."))
              "Autolith discovers and reads standard Skills through cl-skills")
             (test-assert
              (search "call `skill.load`" (skill-catalog-render catalog))
              "Autolith adapts the provider-neutral catalog to skill.load")))
      (skill-tests--delete-root root)
      (skill-tests--delete-root site-root)))
  nil)

(-> skill-tests--extra-root-setting () null)
(defun skill-tests--extra-root-setting ()
  "Test the public extra-root setting, persistence, validation and user init."
  (with-test-configuration (configuration root)
    (let* ((roots       (list (namestring (merge-pathnames "extra/" root))
                              #P"relative-skills/"))
           (application (make-instance 'application
                                       :configuration configuration
                                       :conversation  (conversation-create configuration)
                                       :ui            (terminal-ui-create
                                                       :terminal (make-instance 'recording-terminal)))))
      (test-assert (null (config :extra-skill-roots configuration))
                   "additional skill roots default to an empty list")
      (test-assert
       (eq (application-lisp-evaluation-status
            (application-lisp-evaluate
             (format nil "(settings :extra-skill-roots '~S)" roots)
             :application application))
           ':ok)
       "the public settings operation accepts native root lists")
      (test-assert
       (and (equal (config :extra-skill-roots configuration) roots)
            (equal (config :extra-skill-roots (preferences-tests--create configuration)) roots))
       "string and pathname roots survive preference persistence")
      (let ((saved (uiop:read-file-string (configuration-preferences-path configuration))))
        (dolist (invalid (list 42 "not-a-list" '(nil) '("") '("valid/" 42)
                               (cons "valid/" "not-a-list")))
          (test-assert
           (handler-case
               (progn
                 (application-operation-call application 'settings ':extra-skill-roots invalid)
                 nil)
             (setting-invalid () t))
           "extra skill roots require a proper list of directory designators"))
        (test-assert
         (and (equal (config :extra-skill-roots configuration) roots)
              (string= saved
                       (uiop:read-file-string (configuration-preferences-path configuration))))
         "invalid root lists do not change configuration or preferences"))
      (skill-tests--write
       (config :config-root configuration) "init.lisp"
       "(setf (config :extra-skill-roots) '(\"~/init-skills/\"))")
      (user-init-load configuration)
      (test-assert (equal (config :extra-skill-roots configuration) '("~/init-skills/"))
                   "init.lisp configures the same extra-root setting")
      (application-operation-call application 'settings ':extra-skill-roots nil)
      (test-assert
       (and (null (config :extra-skill-roots configuration))
            (null (config :extra-skill-roots (preferences-tests--create configuration))))
       "NIL clears the additional roots durably")))
  nil)

(-> skill-tests--extra-root-discovery () null)
(defun skill-tests--extra-root-discovery ()
  "Test extra-root order, duplicate directories and string home expansion."
  (with-test-configuration (base root)
    (let* ((home          (merge-pathnames "home/" root))
           (extra-a       (merge-pathnames "extra-a/" home))
           (extra-b       (merge-pathnames "extra-b/" root))
           (source-root   (merge-pathnames "source/" root))
           (bundled       (merge-pathnames "skills/" source-root))
           (configuration (configuration-copy base :source-root source-root)))
      (ensure-directories-exist home)
      (dolist (fixture (list (list bundled "bundled-over-extra" "Bundled instructions.")
                             (list extra-a "bundled-over-extra" "Extra instructions.")
                             (list extra-a "extra-only" "External instructions.")))
        (destructuring-bind (directory name instructions) fixture
          (skill-tests--write directory (format nil "~A/SKILL.sexp" name)
                              (skill-tests--definition name "Extra-root fixture." instructions))))
      (with-test-environment (("HOME" (namestring home))
                              ("USERPROFILE" (namestring home)))
        (let ((standard-roots (skill-roots configuration)))
          (setf (config :extra-skill-roots configuration)
                (list "~/extra-a/" extra-a extra-a (namestring extra-a) bundled extra-b))
          (let ((roots (skill-roots configuration)))
            (test-assert (equal (subseq roots 0 (length standard-roots)) standard-roots)
                         "additional roots follow every standard and bundled root")
            (test-assert (and (= 1 (count extra-a roots :test #'equal))
                              (= 1 (count bundled roots :test #'equal)))
                         "repeated root designators retain their first position"))
          (let* ((catalog  (skill-catalog-for-configuration configuration))
                 (metadata (skill-catalog-find catalog "bundled-over-extra")))
            (test-assert (and metadata (string= (skill-metadata-read metadata) "Bundled instructions."))
                         "a configured extra root does not override bundled skills"))
          (setf (config :extra-skill-roots configuration) '("~/extra-a/"))
          (test-assert (skill-catalog-find (skill-catalog-for-configuration configuration) "extra-only")
                       "a string root expands the isolated user home")
          (setf (config :extra-skill-roots configuration)
                (list (make-pathname :directory '(:relative "~" "extra-a"))))
          (test-assert (null (skill-catalog-find (skill-catalog-for-configuration configuration) "extra-only"))
                       "a pathname root does not expand a literal tilde")))))
  nil)


;;;; -- Ephemeral Selection Tests --

(-> skill-tests--ephemeral-selection () null)
(defun skill-tests--ephemeral-selection ()
  "Test fresh reads, explicit tool selection, stacking, and observability."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (project            (merge-pathnames "project/" root))
         (skill-root         (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-copy
              base-configuration :working-directory
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-ephemeral")))
    (unwind-protect
         (progn
           (skill-tests--write
            skill-root
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha"
             "The alpha skill."
             "Original alpha instructions."))
           (skill-tests--write
            skill-root
            "beta/SKILL.sexp"
            (skill-tests--definition
             "beta"
             "The beta skill."
             "Beta instructions."))
           (let* ((catalog
                    (skill-catalog-for-configuration configuration))
                  (alpha (skill-catalog-find catalog "alpha")))
             (test-assert
              (string=
               (skill-metadata-read alpha)
               "Original alpha instructions.")
              "selected instructions are read on demand")
             (skill-tests--write
              skill-root
              "alpha/SKILL.sexp"
              (skill-tests--definition
               "alpha"
               "A changed description."
               "Replacement alpha instructions."))
             (test-assert
              (string=
               (skill-metadata-read alpha)
               "Replacement alpha instructions.")
              "selected instructions are reparsed fresh from disk")
             (test-assert
              (string=
               (skill-metadata-description alpha)
               "The alpha skill.")
              "catalog metadata does not absorb later file changes"))
           (conversation-append-user-message
            conversation
            "Use alpha and beta and even write skill.load if useful.")
           (let ((outside
                   (skill-request-contributions
                    configuration
                    conversation)))
             (test-assert
              (equal
               (skill-tests--contribution-identifiers outside)
               '("skill-catalog"))
              "durable conversation text never selects a skill"))
           (call-with-skill-logical-turn
            (user-message-input-create
             :text "This text names alpha but does not select it.")
            (lambda ()
              (skill-record-steering-input
               (user-message-input-create
                :text "Steering also names beta without selecting it."))
              (test-assert
                (null (skill-logical-turn-selection-names))
               "initial and steering text do not infer skill selection")
              (multiple-value-bind (metadata new-p)
                  (skill-select-for-logical-turn configuration "beta")
                (test-assert
                 (and new-p
                      (string=
                       (skill-metadata-name metadata)
                       "beta"))
                 "skill.load state selects an exact discovered name"))
              (skill-select-for-logical-turn configuration "alpha")
              (multiple-value-bind (metadata new-p)
                  (skill-select-for-logical-turn configuration "beta")
                (declare (ignore metadata))
                (test-assert
                 (not new-p)
                 "selecting one skill twice is idempotent"))
              (test-assert
                (equal (skill-logical-turn-selection-names)
                       '("beta" "alpha"))
               "multiple skills stack in deterministic selection order")
              (let* ((contributions
                       (skill-request-contributions
                        configuration
                        conversation))
                     (identifiers
                       (skill-tests--contribution-identifiers
                        contributions)))
                (test-assert
                 (equal identifiers
                        '("skill-catalog"
                          "skill-selected-alpha"
                          "skill-selected-beta"))
                 "selected skill instructions stack in catalog order")
                (test-assert
                 (= (length (conversation-input-items conversation)) 1)
                 "skill context never appends durable conversation records"))
              (test-assert
               (null
                (skill-context-contributor
                 (make-instance
                  'request-context
                  :configuration   configuration
                  :conversation    conversation
                  :tool-namespaces #())))
               "restricted child requests without skill.load receive no catalog")
              (test-assert
               (null
                (skill-context-contributor
                 (make-instance
                  'request-context
                  :configuration configuration
                  :conversation  conversation
                  :tool-namespaces
                  (skill-tests--tool-namespaces :load-p nil))))
               "a skill namespace without load does not enable the catalog")
              (test-assert
               (equal
                (skill-tests--contribution-identifiers
                 (skill-context-contributor
                  (make-instance
                   'request-context
                   :configuration configuration
                   :conversation  conversation
                   :tool-namespaces
                   (skill-tests--tool-namespaces))))
                '("skill-catalog"
                  "skill-selected-alpha"
                  "skill-selected-beta"))
               "visible skill.load enables catalog and selected instructions")
              (test-assert
               (skill-context-contributor
                (make-instance
                 'request-context
                 :configuration configuration
                 :conversation  conversation
                 :tool-namespaces
                 (skill-tests--tool-namespaces)
                 :compaction-p  t))
               "compaction requests repeat the skill context that ordinary requests anchor")))
           (test-assert
             (not (skill-logical-turn-active-p))
            "logical-turn selection is dynamically scoped")
           (context-runtime-reset)
           (let* ((*system-prompt-session-date*
                    (cons (current-process-id) "2000-01-01"))
                  (delivery
                    (context-resolve-request
                     configuration
                     conversation
                     (skill-tests--tool-namespaces)))
                  (identifiers
                    (skill-tests--contribution-identifiers
                     (context-delivery-contributions delivery)))
                  (status (context-status conversation)))
              (test-assert
               (every (lambda (identifier)
                        (member identifier identifiers :test #'string=))
                      '("session-state" "date-change" "skill-catalog"))
               (format nil "ordinary context includes session state, date rollover, and the native catalog: ~S~%~A"
                       identifiers status))
              (test-assert
               (notany (lambda (identifier)
                         (member identifier identifiers :test #'string=))
                       '("skill-selected-alpha" "skill-selected-beta"))
               "selected skill instructions expire with the logical turn")
             (test-assert
              (search "skill-catalog" status)
              "/context makes the skill catalog contribution observable"))
           (let ((status (terminal--spans-text (skill-status-entry configuration))))
             (test-assert
              (and (search "alpha" status)
                   (search "beta" status)
                   (search "SKILL.sexp" status))
              "/skills exposes bounded native skill metadata")))
      (context-runtime-reset)
      (skill-tests--delete-root root)))
  nil)

(-> skill-tests--concurrent-parent-child-selection () null)
(defun skill-tests--concurrent-parent-child-selection ()
  "Test concurrent parent and child logical turns keep selections isolated."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (project            (merge-pathnames "project/" root))
         (skills             (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-copy
              base-configuration :working-directory
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-thread-isolation"))
         (barrier-lock       (make-lock "Autolith Skill selection isolation"))
         (barrier            (make-condition-variable))
         (child-ready-p      nil)
         (release-child-p    nil)
         (child-identifiers  nil)
         (child-selection    nil)
         (child-error        nil)
         (child-thread       nil))
    (unwind-protect
         (progn
           (skill-tests--write
            skills
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha" "Parent-only instructions." "Parent body."))
           (skill-tests--write
            skills
            "beta/SKILL.sexp"
            (skill-tests--definition
             "beta" "Child-only instructions." "Child body."))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Parent turn.")
            (lambda ()
              (skill-select-for-logical-turn configuration "alpha")
              (setf child-thread
                    (make-thread
                     (lambda ()
                       (handler-case
                           (call-with-skill-logical-turn
                            (user-message-input-create :text "Child turn.")
                            (lambda ()
                              (skill-select-for-logical-turn
                               configuration
                               "beta")
                              (setf child-selection
                                     (skill-logical-turn-selection-names)
                                    child-identifiers
                                    (skill-tests--contribution-identifiers
                                     (skill-request-contributions
                                      configuration
                                      conversation)))
                              (with-lock-held (barrier-lock)
                                (setf child-ready-p t)
                                (condition-notify barrier)
                                (loop until release-child-p
                                      do
                                         (condition-wait
                                          barrier
                                          barrier-lock)))))
                         (error (condition)
                           (setf child-error condition)
                           (with-lock-held (barrier-lock)
                             (setf child-ready-p t)
                             (condition-notify barrier)))))
                     :name "Autolith child Skill selection isolation"))
              (with-lock-held (barrier-lock)
                (loop until child-ready-p
                      do
                         (condition-wait barrier barrier-lock)))
              (test-assert
                (equal (skill-logical-turn-selection-names) '("alpha"))
               "the active parent turn retains only its own Skill selection")
              (test-assert
               (equal
                (skill-tests--contribution-identifiers
                 (skill-request-contributions configuration conversation))
                '("skill-catalog" "skill-selected-alpha"))
               "parent request context excludes the concurrent child selection")
              (with-lock-held (barrier-lock)
                (setf release-child-p t)
                (condition-notify barrier))))
           (join-thread child-thread)
           (when child-error
             (error child-error))
           (test-assert
            (and (equal child-selection '("beta"))
                 (equal child-identifiers
                        '("skill-catalog" "skill-selected-beta")))
            "the concurrent child turn receives only its own Skill selection")
           (test-assert
             (and (not (skill-logical-turn-active-p))
                  (null (skill-logical-turn-selection-names)))
            "concurrent selection bindings do not escape either logical turn"))
      (when (and child-thread (thread-alive-p child-thread))
        (with-lock-held (barrier-lock)
          (setf release-child-p t)
          (condition-notify barrier))
        (join-thread child-thread))
      (skill-tests--delete-root root)))
  nil)

(-> skill-tests--selection-failures-and-limits () null)
(defun skill-tests--selection-failures-and-limits ()
  "Test selected-file failures and aggregate instruction limits."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (project            (merge-pathnames "project/" root))
         (skills             (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-copy
              base-configuration :working-directory
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-limits")))
    (unwind-protect
         (progn
           (let ((missing-path
                   (skill-tests--write
                    skills
                    "missing/SKILL.sexp"
                    (skill-tests--definition
                     "missing"
                     "This file disappears."
                     "Missing instructions."))))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Select explicitly.")
              (lambda ()
                (skill-select-for-logical-turn configuration "missing")
                (delete-file missing-path)
                (let* ((*skill-warning-character-limit* 96)
                       (contributions
                         (skill-request-contributions
                          configuration
                          conversation))
                       (warning
                         (skill-tests--contribution
                          contributions
                          "skill-warning-missing")))
                  (test-assert
                   (and warning
                        (<=
                         (length
                          (context-contribution-instruction warning))
                         *skill-warning-character-limit*))
                   "a disappearing selected file becomes a bounded warning")
                  (test-assert
                   (null
                    (skill-tests--contribution
                     contributions
                     "skill-selected-missing"))
                   "unreadable selected instructions are never applied")))))
           (skill-tests--write
            skills
            "first/SKILL.sexp"
            (skill-tests--definition
             "first" "First aggregate skill." "First body."))
           (skill-tests--write
            skills
            "second/SKILL.sexp"
            (skill-tests--definition
             "second" "Second aggregate skill." "Second body."))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Select explicitly.")
            (lambda ()
              (skill-select-for-logical-turn configuration "first")
              (skill-select-for-logical-turn configuration "second")
              (let* ((catalog
                       (skill-catalog-for-configuration configuration))
                     (first
                       (skill-catalog-find catalog "first"))
                     (first-size
                       (length
                        (skill--explicit-instruction
                         first
                         (skill-metadata-read first))))
                     (*skill-selection-character-limit* first-size)
                     (contributions
                       (skill-request-contributions
                        configuration
                        conversation)))
                (test-assert
                 (skill-tests--contribution
                  contributions
                  "skill-selected-first")
                 "one skill may exactly fill the aggregate bound")
                (test-assert
                 (skill-tests--contribution
                  contributions
                  "skill-warning-second")
                 "a later skill beyond the aggregate bound becomes a warning")
                (test-assert
                 (null
                  (skill-tests--contribution
                   contributions
                   "skill-selected-second"))
                 "aggregate limits prevent excess instruction injection"))))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Exercise the selection cap.")
            (lambda ()
              (let ((*skill-selection-count-limit* 1))
                (multiple-value-bind (metadata new-p)
                    (skill-select-for-logical-turn configuration "first")
                  (declare (ignore metadata))
                  (test-assert new-p
                               "the first distinct Skill fits the selection cap"))
                (multiple-value-bind (metadata new-p)
                    (skill-select-for-logical-turn configuration "first")
                  (declare (ignore metadata))
                  (test-assert (not new-p)
                               "duplicate selection remains harmless at the cap"))
                (test-assert
                 (handler-case
                     (progn
                       (skill-select-for-logical-turn configuration "second")
                       nil)
                   (skill-selection-error (condition)
                     (eq (skill-selection-error-reason condition)
                         ':selection-limit)))
                 "a distinct Skill beyond the logical-turn cap is rejected")
                (test-assert
                  (equal (skill-logical-turn-selection-names) '("first"))
                 "a rejected over-cap Skill never enters selection state"))))
           (let ((warning-paths nil))
             (dolist (name '("warn-a" "warn-b" "warn-c"))
               (push
                (skill-tests--write
                 skills
                 (format nil "~A/SKILL.sexp" name)
                 (skill-tests--definition
                  name
                  "This selected Skill will disappear."
                  "Transient warning body."))
                warning-paths))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Exercise warning bounds.")
              (lambda ()
                (dolist (name '("warn-a" "warn-b" "warn-c"))
                  (skill-select-for-logical-turn configuration name))
                (dolist (pathname warning-paths)
                  (delete-file pathname))
                (let* ((*skill-warning-character-limit*           96)
                       (*skill-warning-aggregate-character-limit* 120)
                       (contributions
                         (skill-request-contributions
                          configuration
                          conversation))
                       (warnings
                         (remove-if-not
                          #'skill--warning-contribution-p
                          contributions)))
                  (test-assert
                   (and (= (length warnings) 1)
                        (string=
                         (context-contribution-identifier (first warnings))
                         "skill-warning-overflow")
                        (<=
                         (length
                          (context-contribution-instruction (first warnings)))
                         *skill-warning-aggregate-character-limit*))
                   "excess selected-Skill warnings collapse into one bounded mandatory warning")
                  (test-assert
                   (eq (context-contribution-class (first warnings))
                       ':mandatory)
                   "the collapsed warning cannot be dropped by advice budgeting")))))
           (skill-tests--write
            skills
            "oversized/SKILL.sexp"
            (skill-tests--definition
             "oversized"
             "Deferred size failure."
             (make-string 256 :initial-element #\x)))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Select explicitly.")
            (lambda ()
              (skill-select-for-logical-turn configuration "oversized")
              (let* ((*skill-instruction-character-limit* 128)
                     (contributions
                       (skill-request-contributions
                        configuration
                        conversation))
                     (warning
                       (skill-tests--contribution
                        contributions
                        "skill-warning-oversized")))
                (test-assert
                 (and warning
                      (eq (context-contribution-class warning) ':mandatory))
                 "a fresh selected-file size failure becomes an ephemeral warning")))))
      (skill-tests--delete-root root)))
  nil)
