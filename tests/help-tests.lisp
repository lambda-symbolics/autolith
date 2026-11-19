(in-package #:autolith)

;;;; -- Help Fixtures --

(defmacro with-help-tests-application ((application terminal) &body body)
  "Run BODY with a recording application and remove its configuration afterward."
  (let ((root (gensym "ROOT"))
        (tool (gensym "TOOL")))
    `(multiple-value-bind (,application ,root ,terminal ,tool)
         (application-operation-tests--application)
       (declare (ignore ,tool) (ignorable ,terminal))
       (unwind-protect (locally ,@body)
         (terminal-ui-stop (application-ui ,application))
         (platform-delete-directory-tree *platform* ,root
                                         :validate t :if-does-not-exist ':ignore)))))

(-> help-tests--text (list) string)
(defun help-tests--text (items)
  "Return the visible text of Markdown spans and copy widgets."
  (format nil "~{~A~}"
          (mapcar (lambda (item)
                    (if (termdown:widget-p item)
                        (termdown:widget-label item)
                        (terminal-span-text item)))
                  items)))

(-> help-tests--command
    (&key (:definition-name symbol) (:name string) (:aliases list)
          (:help-topic t) (:help-order t))
    application-command)
(defun help-tests--command
    (&key (definition-name 'help-tests--dynamic-command) (name "/commands")
          aliases help-topic help-order)
  "Create a callable fixture command with explicit help metadata."
  (application-command-create
   :definition-name definition-name :name name :aliases aliases :argument nil
   :description "A dynamically registered help test command."
   :tip         "Exercise dynamic help discovery."
   :busy-behavior ':inspect :terminal-behavior ':shared
   :help-topic help-topic :help-order help-order :callable-p t
   :handler     (lambda (application)
                  (declare (ignore application))
                  ':continue)))

(-> help-tests--registration-help (application) null)
(defun help-tests--registration-help (application)
  "Exercise extension ranking, aliases, replacements, and invalid metadata."
  (dolist (command
           (list (help-tests--command :definition-name 'help-tests--zeta-command
                                      :name            "/help-zeta"
                                      :help-topic ':channels :help-order 0)
                 (help-tests--command :definition-name 'help-tests--alpha-command
                                      :name "/help-alpha" :aliases '("/help-alias")
                                      :help-topic ':channels :help-order 0)
                 (help-tests--command :definition-name 'help-tests--unranked-command
                                      :name "/help-unranked" :help-topic ':channels)))
    (register-application-command command :source ':test))
  (let* ((section   (application-operation-help application ':channels))
         (positions (mapcar (lambda (usage) (search usage section))
                            '("(help-zeta)" "(help-alpha)" "(open" "(help-unranked)"))))
    (test-assert (and (every #'integerp positions) (apply #'< positions))
                 "topic extensions sort by rank, retain effective-registration ties, and place unranked commands last")
    (test-assert (and (not (search "(help-alias)" section))
                      (equal (application-operation-help application 'help-alias)
                             (application-operation-help application 'help-alpha)))
                 "aliases resolve to their canonical command without another section listing"))
  (register-application-command
   (help-tests--command :definition-name 'help-tests--replacement-command :name "/open")
   :source ':test)
  (let* ((channels    (application-operation-help application ':channels))
         (commands    (application-operation-help application ':commands))
         (other       (search "## Other commands" commands))
         (replacement (search "(open)" commands)))
    (test-assert (and (not (search "(open" channels))
                      other replacement (< other replacement))
                 "a replacement without metadata belongs to Other commands instead of inheriting builtin metadata"))
  (register-application-command
   (help-tests--command :definition-name 'help-tests--replacement-command :name "/open"
                        :help-topic ':workspace :help-order -1)
   :source ':test)
  (let* ((workspace   (application-operation-help application ':workspace))
         (replacement (search "(open)" workspace))
         (directory   (search "(cwd" workspace)))
    (test-assert (and replacement directory (< replacement directory)
                      (not (search "(open" (application-operation-help application ':channels))))
                 "a replacement supplies its own topic and rank"))
  (dolist (metadata '((:help-topic :unknown-help-topic)
                      (:help-topic "channels")
                      (:help-order "first")
                      (:help-order 1/2)))
    (test-assert
     (handler-case (progn (apply #'help-tests--command metadata) nil)
       (configuration-error () t))
     "constructors refuse invalid help metadata with a configuration condition")
    (test-assert
     (handler-case
         (progn
           (macroexpand-1
            `(define-application-command help-tests--invalid-command
                 (:name "/help-invalid" :argument nil
                  :description "Invalid help metadata fixture."
                  :tip         "Exercise definition refusal."
                  :busy-behavior :inspect :terminal-behavior :shared
                  :callable    t ,@metadata)
                 (application)
               (declare (ignore application))
               ':continue))
           nil)
       (application-command-definition-error () t))
     "defining forms refuse invalid help metadata with a definition condition"))
  nil)


;;;; -- Selection and Discovery --

(-> test-help-selection-and-bounds () null)
(defun test-help-selection-and-bounds ()
  "Test progressive disclosure, section precedence, aliases, and live tool schemas."
  (with-help-tests-application (application terminal)
    (let* ((overview (application-operation-help application))
           (section  (application-operation-help application ':channels))
           (command  (application-operation-help application 'open))
           (tool     (application-operation-help application "test-operation.echo")))
      (test-assert (and (search "(new)" section) (not (search "(new)" command)))
                   "keywords choose sections while quoted symbols choose commands")
      (let ((positions (mapcar (lambda (usage) (search usage section))
                               '("(open" "(new)" "(tell" "(move" "(dismiss" "(domestic)" "(rename"))))
        (test-assert (and (every #'integerp positions) (apply #'< positions))
                     "channels help includes canonical open and preserves the editorial command order"))
      (test-assert (equal (application-operation-help application " OpEn ")
                          (application-operation-help application 'open))
                   "command subjects accept strings and ignore case and outer whitespace")
      (test-assert (equal (application-operation-help application 'usage)
                          (application-operation-help application 'status))
                   "help can resolve an existing command alias")
      (let* ((runtime (application-operation-help application ':runtime))
             (status  (search "(status)" runtime)))
        (test-assert (and status (not (search "(status)" runtime :start2 (1+ status)))
                          (not (search "(usage)" runtime)))
                     "runtime help lists the canonical status command once for its aliases"))
      (test-assert (search "test-operation"
                           (application-operation-help application ':tools))
                   "the tools index exposes live namespaces")
      (test-assert (search "test-operation.echo"
                           (application-operation-help application "test-operation"))
                   "namespace help exposes its callable tools")
      (test-assert (and (search "Text to echo." tool)
                        (search "required" tool)
                        (search "optional" tool)
                        (search ":text" tool))
                   "individual tool help includes current parameter documentation and obligations")
      (tool-registry-register
       (application-tool-registry application)
       (make-instance 'tool :namespace "large-help" :name "entry"
                           :description (make-string 4096 :initial-element #\x)
                           :parameters  (tool-object-schema (json-object) nil)))
      (test-assert (<= (length (application-operation-help application)) (length overview))
                   "adding a large tool description does not enlarge the overview")
      (test-assert (search "large-help.entry" (application-operation-help application ':all))
                   "the explicit full reference includes newly registered tools")))
  nil)

(-> test-help-dynamic-discovery-and-completion () null)
(defun test-help-dynamic-discovery-and-completion ()
  "Test unclassified live commands and contextual help-subject completions."
  (with-help-tests-application (application terminal)
    (let ((snapshot (application-command--registry-snapshot)))
      (unwind-protect
           (progn
             (register-application-command (help-tests--command) :source ':test)
             (let ((section (application-operation-help application ':commands))
                   (command (application-operation-help application 'commands)))
               (let ((other   (search "## Other commands" section))
                     (dynamic (search "(commands)" section)))
                 (test-assert (and other dynamic (< other dynamic))
                              "commands without help metadata appear under Other commands"))
               (test-assert
                (and (search "(new)" section)
                     (not (search "(new)" command))
                     (equal command (application-operation-help application "COMMANDS")))
                "reserved keyword topics take precedence over live commands"))
             (help-tests--registration-help application)
             (let* ((ui     (application-ui application))
                    (editor (terminal-ui-editor ui)))
               (test-assert
                 (= 1 (count "(help \"commands\")"
                            (application-operation-completion-entries application)
                            :test #'string= :key (lambda (entry) (getf entry :name))))
                "a topic and command collision produces one ergonomic completion")
               (setf (terminal-interactive-p terminal) t)
               (line-editor-set-text editor "(h")
               (test-assert
                (not (find "(help :workspace)" (terminal-ui--matching-completions ui)
                           :test #'string= :key (lambda (entry) (getf entry :name))))
                "help subjects are hidden until the command's argument position")
               (line-editor-set-text editor "(help ")
               (let ((matches (terminal-ui--matching-completions ui)))
                 (dolist (name '("(help :workspace)" "(help \"commands\")"
                                 "(help \"test-operation.echo\")"))
                   (let ((entry (find name matches :test #'string=
                                                   :key  (lambda (entry) (getf entry :name)))))
                     (test-assert entry "help completion offers live subjects after the command")
                     (when entry
                       (terminal-ui--accept-completion ui entry)
                       (test-assert (equal (line-editor-text editor) name)
                                    "accepting a help subject inserts its complete Lisp form")))))))
        (application-command--registry-restore snapshot))))
  nil)


;;;; -- Public Invocation and Rendering --

(-> test-help-public-invocation () null)
(defun test-help-public-invocation ()
  "Test native help invocation, readable errors, and provider isolation."
  (with-help-tests-application (application terminal)
    (let* ((conversation (application-conversation application))
           (before       (conversation-next-sequence conversation)))
      (dolist (source '("(help)" "(help :workspace)" "(help :resume)" "(help 'open)"
                        "(help \"test-operation.echo\")" "(help 42)"
                        "(help \"definitely-unknown\")"))
        (recording-terminal-reset terminal)
        (let ((evaluation (application-lisp-evaluate source :application application)))
          (test-assert (eq (application-lisp-evaluation-status evaluation) ':ok)
                       "help subjects and invalid-subject guidance finish without a debugger")
          (test-assert (plusp (length (recording-terminal-output terminal)))
                       "help presents its result in the terminal")))
      (test-assert (= before (conversation-next-sequence conversation))
                   "help output does not append provider conversation records")
      (test-assert (zerop (application-operation-test-tool-calls
                           (tool-registry-find (application-tool-registry application)
                                               "test-operation" "echo")))
                   "looking up a tool never invokes it")
      (dolist (case '(("(help)" :execute)
                      ("(help :workspace)" :execute)
                      ("(help 'open)" :execute)
                      ("(help \"test-operation.echo\")" :execute)
                      ("(help (read-line))" :hold)))
        (destructuring-bind (source expected) case
          (test-assert (eq (application-operation-source-active-turn-action application source)
                           expected)
                       "literal help may run during a turn, while computed arguments wait")))))
  nil)

(-> help-tests--heading-rendering (terminal) null)
(defun help-tests--heading-rendering (terminal)
  "Exercise heading styles through the shared Markdown and terminal boundaries."
  (setf (terminal-styled-p terminal) t)
  (dolist (theme *terminal-themes*)
    (let ((*terminal-style-table* (terminal-theme-style-table theme)))
      (loop for level from 1 to 6
            for source = (format nil "~A Heading with *emphasis* and `code`"
                                 (make-string level :initial-element #\#))
            for renderer = (termdown:markdown-renderer-create :width 24)
            for rows = (terminal-markdown-render-line renderer source)
            for spans = (apply #'append rows)
            for styles = (mapcar #'terminal-span-style spans)
            do (test-assert (every #'terminal-styled-text-p rows)
                            "heading rows use registered semantic styles")
               (test-assert (and (member ':syntax-heading styles)
                                 (member ':heading-emphasis styles)
                                 (member ':code styles))
                            "wrapped headings retain colored text, italic emphasis, and inline code")
               (test-assert
                (equal (mapcar #'terminal--spans-text rows)
                       (mapcar #'terminal--spans-text
                               (termdown:markdown-render-line
                                (termdown:markdown-renderer-create :width 24) source)))
                "heading colors preserve hierarchy markers, text, and wrapping")
               (let* ((terminal-heading (find ':syntax-heading spans
                                              :key #'terminal-span-style))
                      (display          (terminal--render-spans terminal (list terminal-heading))))
                 (test-assert (search (terminal-style-sequence ':syntax-heading) display)
                              "styled terminals render heading colors from the current theme"))
               (test-assert
                (every (lambda (row) (<= (terminal--spans-width row) 24)) rows)
                "colored heading rows fit narrow viewports"))))
  (let ((renderer (termdown:markdown-renderer-create :width 40)))
    (dolist (source '("ordinary **strong** text" "#not-a-heading" "####### seven"))
      (test-assert
       (not (find ':syntax-heading
                  (apply #'append (terminal-markdown-render-line renderer source))
                  :key #'terminal-span-style))
       "ordinary prose and nonheading hash prefixes do not receive heading color"))
    (terminal-markdown-render-line renderer "```text")
    (test-assert
     (not (find ':syntax-heading
                (apply #'append (terminal-markdown-render-line renderer "# literal code"))
                :key #'terminal-span-style))
     "hash-prefixed fenced source keeps its code styles"))
  nil)

(-> test-help-markdown-rendering () null)
(defun test-help-markdown-rendering ()
  "Test semantic colors, Lisp highlighting, copy widgets, and narrow-terminal wrapping."
  (with-help-tests-application (application terminal)
    (let* ((document (application-operation-help application))
           (items    (application--markdown-body application document))
           (styles   (mapcar #'terminal-span-style (remove-if #'termdown:widget-p items))))
      (test-assert (and (member ':brand styles) (member ':code styles)
                        (member ':syntax-heading styles))
                   "help uses semantic colors for its menu and inline command forms")
      (test-assert (member ':syntax-function styles)
                   "Lisp examples go through the syntax highlighter")
      (test-assert (find-if #'termdown:widget-p items)
                   "rendered examples include source-copy widgets")
      (dolist (width '(40 80))
        (setf (terminal-columns terminal) width)
        (let* ((text  (help-tests--text (application--markdown-body application document)))
               (lines (uiop:split-string text :separator '(#\Newline))))
          (test-assert (every (lambda (line) (<= (text-cell-width line) width)) lines)
                       "help prose and examples fit the current terminal width")))))
  (with-help-tests-application (application terminal)
    (help-tests--heading-rendering terminal))
  nil)
