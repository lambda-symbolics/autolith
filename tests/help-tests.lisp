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


;;;; -- Selection and Discovery --

(-> test-help-selection-and-bounds () null)
(defun test-help-selection-and-bounds ()
  "Test progressive disclosure, section precedence, aliases, and live tool schemas."
  (with-help-tests-application (application terminal)
    (let* ((overview (application-operation-help application))
           (section  (application-operation-help application ':conversations))
           (command  (application-operation-help application 'conversations))
           (tool     (application-operation-help application "test-operation.echo")))
      (test-assert (and (search "(new)" section) (not (search "(new)" command)))
                   "keywords choose sections while quoted symbols choose commands")
      (test-assert (equal (application-operation-help application " SwItCh ")
                          (application-operation-help application 'switch))
                   "command subjects accept strings and ignore case and outer whitespace")
      (test-assert (equal (application-operation-help application "/usage")
                          (application-operation-help application 'status))
                   "help can resolve an existing command alias")
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
             (register-application-command
              (application-command-create
               :definition-name 'help-tests--dynamic-command
               :name "/commands" :aliases nil :argument nil
               :description     "A dynamically registered help test command."
               :tip             "Exercise dynamic help discovery."
               :busy-behavior ':inspect :terminal-behavior ':shared
               :lambda-list nil :callable-p t :static-options nil
               :handler         (lambda (application)
                                  (declare (ignore application))
                                  ':continue))
              :source ':test)
             (let ((section (application-operation-help application ':commands))
                   (command (application-operation-help application 'commands)))
               (test-assert (search "(commands)" section)
                            "new commands appear even without a curated section")
               (test-assert
                (and (search "(new)" section)
                     (not (search "(new)" command))
                     (equal command (application-operation-help application "COMMANDS")))
                "reserved keyword topics take precedence over live commands"))
             (let* ((ui     (application-ui application))
                    (editor (terminal-ui-editor ui)))
               (test-assert
                (= 1 (count "/help commands"
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
  "Test Lisp and ergonomic command invocation, readable errors, and provider isolation."
  (with-help-tests-application (application terminal)
    (let* ((conversation (application-conversation application))
           (before       (conversation-next-sequence conversation)))
      (dolist (source '("(help)" "(help :workspace)" "(help 'switch)"
                        "(help \"test-operation.echo\")" "(help 42)"
                        "(help \"definitely-unknown\")"))
        (recording-terminal-reset terminal)
        (let ((evaluation (application-lisp-evaluate source :application application)))
          (test-assert (eq (application-lisp-evaluation-status evaluation) ':ok)
                       "help subjects and invalid-subject guidance finish without a debugger")
          (test-assert (plusp (length (recording-terminal-output terminal)))
                       "help presents its result in the terminal")))
      (dolist (input '("/help workspace" "/help resume" "/help definitely-unknown"))
        (test-assert (eq (application--run-command-input application input) ':continue)
                     "ergonomic help syntax accepts topics and unknown-subject guidance"))
      (test-assert (= before (conversation-next-sequence conversation))
                   "help output does not append provider conversation records")
      (test-assert (zerop (application-operation-test-tool-calls
                           (tool-registry-find (application-tool-registry application)
                                               "test-operation" "echo")))
                   "looking up a tool never invokes it")
      (dolist (case '(("(help)" :execute)
                      ("(help :workspace)" :execute)
                      ("(help 'switch)" :execute)
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
