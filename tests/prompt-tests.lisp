(in-package #:autolith)

;;;; -- System Prompt Tests --

(-> prompt-tests--contains (string string string) null)
(defun prompt-tests--contains (text snippet description)
  "Assert TEXT contains SNIPPET."
  (test-assert (search snippet text) description))

(-> prompt-tests--absent (string string string) null)
(defun prompt-tests--absent (text snippet description)
  "Assert TEXT does not contain SNIPPET."
  (test-assert (not (search snippet text)) description))

(-> test-system-prompt-structured-knowledge () null)
(defun test-system-prompt-structured-knowledge ()
  "Test the prompt explains structured knowledge and diagnosis only while they are offered."
  (dolist (case '(("active" nil t nil "active knowledge explains facts, rules, and contracts")
                  ("active" t t t "enabled diagnosis adds its guidance")
                  ("off" t nil nil "knowledge off leaves both sections out")))
    (destructuring-bind (mode diagnosis-p knowledge-p diagnosis-shown-p description) case
      (with-test-configuration (configuration)
        (setf (config :knowledge-mode configuration) mode
              (config :diagnosis-p configuration)    diagnosis-p)
        (let ((prompt (system-prompt configuration)))
          (test-assert (and (eq (not (null (search "(admit-rule)" prompt))) knowledge-p)
                            (eq (not (null (search "diagnosis.run" prompt))) diagnosis-shown-p))
                       description)))))
  nil)

(-> test-system-prompt-child-role () null)
(defun test-system-prompt-child-role ()
  "Test a child's role and its instructions reach the stable prompt, and an agent nobody spawned has no role section."
  (with-test-configuration (configuration)
    (let* ((definition (make-instance 'task-agent-definition
                                      :name         "reviewer"
                                      :description  "Review changes."
                                      :instructions "Review the change for correctness first."
                                      :source       ':test))
           (child      (system-prompt (task-configuration-for-definition configuration definition)))
           (primary    (system-prompt configuration)))
      (prompt-tests--contains child "role reviewer."
                              "a child's prompt names its role")
      (prompt-tests--contains child "Review the change for correctness first."
                              "a child's prompt carries its role's instructions")
      (prompt-tests--absent primary "spawned you with"
                            "an agent nobody spawned has no role section")))
  nil)

(-> test-system-prompt () null)
(defun test-system-prompt ()
  "Test stable system-prompt and mutable request-context rendering."
  (let ((configuration (test-configuration)))
    (unwind-protect
         (progn
           (test-assert (probe-file (system-prompt--template-path))
                        "the Org system prompt template is shipped with Autolith")
           (test-assert (probe-file (request-context--template-path))
                        "the Org request-context template is shipped with Autolith")
           (test-assert
            (string= (request-context--bounded-complete-lines
                      (format nil "first row~%second row is too long") 25)
                     (format nil "first row~%... [truncated]"))
            "mutable context bounds only at complete row boundaries")
           (let ((prompt  (system-prompt configuration))
                 (context (request-context-session-state configuration)))
             (prompt-tests--contains prompt "You are Autolith"
                                     "the rendered prompt keeps the Autolith identity")
             (prompt-tests--contains context "Current workspace agenda: empty."
                                     "mutable context describes an empty agenda")
             (prompt-tests--contains context "Saved Lisp worker images"
                                     "mutable context carries worker-image state")
             (prompt-tests--absent prompt "Current workspace agenda"
                                   "agenda state stays outside the stable prompt")
             (prompt-tests--contains prompt "Your main power is the live image"
                                     "a mutable session uses the live-image section")
             (prompt-tests--contains prompt "Use web.run"
                                     "cached web search advertises web.run")
             (prompt-tests--contains prompt (system-prompt-session-date)
                                     "the prompt embeds the session date")
             (prompt-tests--contains prompt "Workspace instructions from"
                                     "workspace AGENTS.md is included")
             (dolist (rendered (list prompt context))
               (prompt-tests--absent rendered "{{{:"
                                     "no leftover org-templater slots remain")
               (prompt-tests--absent rendered "#+TITLE"
                                     "Org keyword lines are stripped")
               (prompt-tests--absent rendered "* Commentary"
                                     "review commentary is omitted"))
             (prompt-tests--contains prompt "RECURSIVE INFERENCE IS AVAILABLE"
                                     "RLM guidance rides with registered rlm tools")
             (prompt-tests--absent context "SIMPLE TECHNICAL ENGLISH MODE IS ACTIVE"
                                   "STE is omitted when the preference is off")
             (prompt-tests--absent context "HURRY-UP MODE IS ACTIVE"
                                   "hurry-up is omitted unless requested")
             (prompt-tests--absent prompt "hosted web_search"
                                   "hosted search is omitted unless the request hosts it")
             (prompt-tests--absent prompt "This session was started with --immutable"
                                   "immutable guidance is omitted for a live image"))
           (let ((prompt (let ((*system-prompt-hosted-web-search-p* t))
                           (system-prompt configuration))))
             (prompt-tests--contains prompt "hosted web_search"
                                     "hosted search guidance follows the request binding")
             (prompt-tests--absent prompt "Use web.run"
                                   "hosted search takes precedence over web.run"))
           (let ((context (request-context-session-state
                           configuration :hurry-up-p t)))
             (prompt-tests--contains context "HURRY-UP MODE IS ACTIVE"
                                     "hurry-up guidance rides in mutable context"))
           (setf (config :web-search-mode configuration) "disabled")
           (let ((prompt (system-prompt configuration)))
             (prompt-tests--absent prompt "WEB SEARCH IS AVAILABLE"
                                   "disabled web search omits both search vehicles"))
           (setf (config :immutable-p configuration) t)
           (let ((prompt (system-prompt configuration)))
             (prompt-tests--contains prompt "This session was started with --immutable"
                                     "an immutable session uses its prompt section")
             (prompt-tests--contains prompt "The self namespace is inspection-only"
                                     "an immutable session uses inspection-only tools")
             (prompt-tests--absent prompt "Your main power is the live image"
                                   "an immutable session omits live-image guidance"))
           (setf (config :immutable-p configuration) nil)
           (let ((stable-prompt (system-prompt configuration)))
             (setf (config :simple-technical-english-p configuration) t)
             (test-assert (string= stable-prompt (system-prompt configuration))
                          "an STE toggle does not rewrite the stable system prompt")
             (prompt-tests--contains
              (request-context-session-state configuration)
              "SIMPLE TECHNICAL ENGLISH MODE IS ACTIVE"
              "STE guidance rides in mutable context")))
      (platform-delete-directory-tree *platform* (test-configuration-root configuration)
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-system-prompt-session-date () null)
(defun test-system-prompt-session-date ()
  "Keep the prompt's date for the process and report a later date as separate context."
  (with-test-configuration (configuration)
    (let* ((today        (system-prompt--current-date))
           (conversation (conversation-create configuration))
           (request      (make-instance 'request-context
                                        :configuration   configuration
                                        :conversation    conversation
                                        :tool-namespaces #())))
      (let ((*system-prompt-session-date* (cons (current-process-id) "2000-01-01")))
        (test-assert (search "2000-01-01" (system-prompt configuration))
                     "the prompt keeps the session date after midnight")
        (test-assert (not (search today (system-prompt configuration)))
                     "the prompt does not change with the current date")
        (test-assert (equal (system-prompt-date-update) today)
                     "a moved date is reported")
        (let ((contribution (context--date-change request)))
          (test-assert (and contribution
                            (search today (context-contribution-instruction contribution))
                            (search "2000-01-01" (context-contribution-instruction contribution)))
                       "the date change arrives as its own context contribution")))
      (let ((*system-prompt-session-date* (cons (1+ (current-process-id)) "2000-01-01")))
        (test-assert (equal (system-prompt-session-date) today)
                     "another process takes the date of its own start")
        (test-assert (and (null (system-prompt-date-update))
                          (null (context--date-change request)))
                     "an unchanged date adds no context"))))
  nil)

(-> test-request-context-agenda-selection () null)
(defun test-request-context-agenda-selection ()
  "Test request context includes active agenda work but omits completed history."
  (let ((configuration (test-configuration)))
    (unwind-protect
         (let ((state (agenda-load configuration)))
           (agenda-add :configuration configuration
                       :state         state
                       :text          "completed-history-marker"
                       :status        ':done)
           (dolist (status '(:todo :doing :blocked :note))
             (agenda-add :configuration configuration
                         :state         state
                         :text          (format nil "active-~(~A~)-marker" status)
                         :status        status))
           (let ((lines (agenda-prompt-item-lines configuration)))
             (test-assert
              (every (lambda (status)
                       (search (format nil "active-~(~A~)-marker" status)
                               lines))
                     '(:todo :doing :blocked :note))
              "request context carries every active agenda status")
             (test-assert
              (not (search "completed-history-marker" lines))
              "request context omits completed agenda history"))
           (let* ((*request-context-agenda-limit* 96)
                  (context                        (request-context--config configuration))
                  (agenda                         (gethash ':agenda context)))
             (test-assert
              (and (<= (length agenda) *request-context-agenda-limit*)
                   (search *system-prompt-context-truncation-marker* agenda))
              "active agenda rendering obeys the complete-line context bound")))
      (platform-delete-directory-tree *platform* (test-configuration-root configuration)
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)
