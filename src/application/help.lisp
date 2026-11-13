(in-package #:autolith)

;;;; -- Human Help --

(defparameter *application-help-sections*
  '((:topic :channels :title "Channels"
     :description "Move between channels, message them, and place their agents."
     :commands    ("switch" "new" "tell" "move" "dismiss" "domestic" "rename"))
    (:topic :conversations :title "Conversations"
     :description "Manage the conversation of this channel."
     :commands    ("history" "goal" "context" "compact"))
    (:topic :models :title "Models"
     :description "Choose a provider, model, and reasoning effort."
     :commands    ("auth" "model" "models" "effort" "fast" "hurry-up"))
    (:topic :workspace :title "Workspace"
     :description "Change directories, permissions, skills, and workspace notes."
     :commands    ("cwd" "permissions" "agenda" "skills" "mcp" "papercuts"
                   "papercut" "papercut-close"))
    (:topic :display :title "Display"
     :description "Adjust settings, reasoning, timestamps, and transcript detail."
     :commands    ("onboard" "settings" "trace" "timestamps" "ste" "titles" "cache-misses"
                   "compact-tool"))
    (:topic :runtime :title "Runtime and recovery"
     :description "Inspect the session, save a generation, or recover queued input."
     :commands    ("info" "status" "checkpoint" "checkpoint-cancel" "generations" "rollback" "update"
                   "fix-skipped-definitions" "vault" "vault-store" "vault-restore"
                   "vault-discard" "data.export" "data.import" "detach" "quit")))
  "Human help topics and their preferred command order; registrations supply the text.")

(defgeneric application-help-operation-notes (operation)
  (:documentation "Return OPERATION's detailed Markdown notes from its live backend."))

(defmethod application-help-operation-notes ((operation application-command-operation))
  "Describe a command using its registered human guidance."
  (application-command-tip (application-operation-backend operation)))

(defmethod application-help-operation-notes ((operation application-local-operation))
  "Use a local Lisp operation's function or macro documentation."
  (let ((name (application-operation-backend operation)))
    (or (documentation name 'function) "")))

(defmethod application-help-operation-notes ((operation application-tool-operation))
  "Describe the required and optional parameters in a tool's current schema."
  (let* ((tool       (application-operation-backend operation))
         (schema     (tool-parameters tool))
         (properties (and (json-object-p schema) (json-get schema "properties"))))
    (multiple-value-bind (required optional)
        (application-operation--tool-property-names tool)
      (with-output-to-string (stream)
        (when (or required optional)
          (format stream "## Arguments~%~%")
          (dolist (name (append required optional))
            (let* ((property    (json-get properties name))
                   (description (and (json-object-p property)
                                     (json-get property "description")))
                   (type        (and (json-object-p property) (json-get property "type"))))
              (format stream "- ~A **~A**~@[ · ~A~]~%~@[  ~A~%~]"
                      (application-help--inline-code
                       (application--lisp-key-token name))
                      (if (member name required :test #'string=)
                          "required"
                          "optional")
                      (and (stringp type) (application-help--inline-code type))
                      (and (stringp description) description)))))))))

(-> application-operation-help (application &optional t) string)
(defun application-operation-help (application &optional topic)
  "Return a compact Markdown overview, a TOPIC section, or one operation's help.

Keyword topics select sections before commands. Other symbols and strings select
commands before sections. Tool namespaces and command aliases are also accepted."
  (if (null topic)
      (application-help--overview)
      (let* ((operations (application-operation-list application))
             (name       (application-help--topic-name topic))
             (section-p
               (or (keywordp topic)
                   (and (stringp topic)
                        (uiop:string-prefix-p
                         ":" (string-left-trim '(#\Space #\Tab #\Newline #\Return) topic)))))
             (operation  (and name (application-help--find-operation name operations)))
             (section    (and name
                              (or section-p (null operation))
                              (application-help--topic-document name operations))))
        (cond
          ((and section-p section)
           section)
          (operation
           (application-help--operation-document operation))
          (section
           section)
          (t
           (format nil "# Help~%~%~A~%~%~A"
                   (if name
                        (format nil "No help found for ~A."
                                (application-help--inline-code name))
                       "Choose a section keyword or a command name.")
                   (application-help--topic-menu)))))))

(-> application-help-completion-entries (application) list)
(defun application-help-completion-entries (application)
  "Return live Lisp and slash help-subject completions behind the help command."
  (let ((command (application-command-find "/help")))
    (when (and command
               (eq (application-command-definition-name command)
                   'application--builtin-help-command))
      (let* ((operations (application-operation-list application))
             (topics     (append (mapcar (lambda (entry) (getf entry :topic))
                                         *application-help-sections*)
                                 '(:local :commands :tools :all)))
             (namespaces (application-help--tool-namespaces operations)))
        (remove-duplicates
         (append
          (loop for topic in topics
                for name = (string-downcase (symbol-name topic))
                append (list (list :name    (format nil "(help :~A)" name)
                                   :argument nil :description "browse a help section"
                                   :primary "(help")
                             (list :name    (format nil "/help ~A" name)
                                   :argument nil :description "browse a help section"
                                   :primary "/help")))
          (loop for name in (remove-duplicates
                             (append namespaces
                                     (mapcar #'application-operation-name operations))
                             :test #'string-equal)
                append (list (list :name    (format nil "(help ~S)" name)
                                   :argument nil :description "show help for this subject"
                                   :primary "(help")
                             (list :name    (format nil "/help ~A"
                                                    (application-command--slash-option-token name))
                                   :argument nil :description "show help for this subject"
                                   :primary "/help"))))
         :test #'string= :key (lambda (entry) (getf entry :name)))))))


;;;; -- Documents --

(-> application-help--overview () string)
(defun application-help--overview ()
  "Return the short entry page with executable Lisp examples and topic links."
  (format nil "# Autolith help

Talk to Autolith in plain text. Run local commands as Lisp forms.

## Everyday commands

```lisp
(open)  ; go to another channel
(new)     ; open a new channel here
(model)   ; choose a model
(cwd)     ; show workspace
```

~A

For one command, use `(help 'switch)`. For a tool, use `(help \"lisp.eval\")`."
          (application-help--topic-menu)))

(-> application-help--topic-menu () string)
(defun application-help--topic-menu ()
  "Return the topic menu shared by the overview and unknown-subject guidance."
  (with-output-to-string (stream)
    (format stream "## Help topics~%~%")
    (dolist (section *application-help-sections*)
      (format stream "- **~A** · `(help :~(~A~))`~%"
              (getf section :title) (getf section :topic)))
    (format stream "
Local Lisp: `(help :local)` · Tools: `(help :tools)`

All commands: `(help :commands)` · Full reference: `(help :all)`")))

(-> application-help--topic-document (string list) (option string))
(defun application-help--topic-document (name operations)
  "Return a named section or tool namespace document, or NIL when unknown."
  (let ((section (find name *application-help-sections*
                           :test #'string-equal
                           :key  (lambda (entry) (symbol-name (getf entry :topic))))))
    (cond
      (section
       (format nil "~A~%~%Use `(help 'NAME)` for a command's usage and details."
               (application-help--section-document section operations)))
      ((string= name "commands")
       (application-help--command-document operations))
      ((string= name "tools")
       (application-help--tool-index operations))
      ((string= name "local")
       (application-help--operation-list
        "Local Lisp" (application-help--operations-of-kind operations ':local)))
      ((string= name "all")
       (format nil "~A~%~%~A~%~%~A"
               (application-help--command-document operations)
               (application-help--operation-list
                "Local Lisp" (application-help--operations-of-kind operations ':local))
               (application-help--operation-list
                "Tools" (application-help--operations-of-kind operations ':tool))))
      ((find name (application-help--tool-namespaces operations) :test #'string-equal)
       (application-help--operation-list
        (format nil "~A tools" name)
        (remove-if-not
         (lambda (operation)
           (string-equal name (tool-namespace (application-operation-backend operation))))
         (application-help--operations-of-kind operations ':tool))))
      (t
       nil))))

(-> application-help--operations-of-kind (list keyword) list)
(defun application-help--operations-of-kind (operations kind)
  "Select one operation family through its CLOS kind protocol."
  (remove-if-not (lambda (operation) (eq (application-operation-kind operation) kind))
                 operations))

(-> application-help--section-document (list list) string)
(defun application-help--section-document (section operations)
  "Return SECTION's currently registered commands in the preferred order."
  (application-help--operation-list
   (getf section :title)
   (loop for name in (getf section :commands)
         for operation = (find name operations :test #'string=
                                              :key   #'application-operation-name)
         when (typep operation 'application-command-operation) collect operation)
   (getf section :description)))

(-> application-help--command-document (list) string)
(defun application-help--command-document (operations)
  "List all commands, including registrations outside the curated topics."
  (let* ((commands (application-help--operations-of-kind operations ':command))
         (covered  (loop for section in *application-help-sections*
                         append (getf section :commands)))
         (other    (remove-if (lambda (entry)
                                (member (application-operation-name entry) covered
                                        :test #'string=))
                              commands)))
    (format nil "# Commands~%~%Use `(help 'NAME)` for usage and details.~%~%~{~A~^~%~%~}~@[~%~%~A~]"
            (loop for section in *application-help-sections*
                  collect (application-help--section-document section operations))
            (and other (application-help--operation-list "Other commands" other)))))

(-> application-help--operation-list (string list &optional (option string)) string)
(defun application-help--operation-list (title operations &optional description)
  "Return a Markdown list with an optional section DESCRIPTION."
  (with-output-to-string (stream)
    (format stream "## ~A~%~%~@[~A~%~%~]" title description)
    (if operations
        (dolist (operation operations)
          (format stream "- ~A~%  ~A~%"
                  (application-help--inline-code
                   (terminal-completion-label (application-operation-completion-entry operation)))
                  (application-help--summary operation)))
        (format stream "No operations in this section."))))

(-> application-help--operation-document (application-operation) string)
(defun application-help--operation-document (operation)
  "Return one operation's description, highlighted usage, and backend notes."
  (let ((notes (application-help-operation-notes operation))
        (usage (application-help--usage operation)))
    (format nil "# ~A

~A

## Usage

```lisp
~A
```~@[~%~%~A~]~@[~%~%~A~]"
            (application-operation-name operation)
            (application-operation-description operation)
            usage
            (and (find #\[ usage) "Arguments in brackets are optional.")
            (and (plusp (length notes)) notes))))

(-> application-help--usage (application-operation) string)
(defun application-help--usage (operation)
  "Return a usage form, placing tool keyword arguments on separate lines."
  (if (typep operation 'application-tool-operation)
      (multiple-value-bind (required optional)
          (application-operation--tool-property-names (application-operation-backend operation))
        (format nil "(~A~{~%  ~A~})"
                (application-operation-name operation)
                (loop for name in (append required optional)
                      collect (application-operation--tool-argument-fragment
                               name (not (null (member name required :test #'string=)))))))
      (terminal-completion-label (application-operation-completion-entry operation))))

(-> application-help--tool-index (list) string)
(defun application-help--tool-index (operations)
  "List callable tool namespaces rather than dumping every tool's schema."
  (let ((namespaces (application-help--tool-namespaces operations)))
    (with-output-to-string (stream)
      (format stream "# Tools

Choose a namespace or a tool name, for example `(help :lisp)` or `(help \"lisp.eval\")`.

")
      (if namespaces
          (dolist (namespace namespaces)
            (format stream "- ~A · ~D tool~:P · ~A~%"
                    (application-help--inline-code namespace)
                    (count-if (lambda (entry)
                                (and (typep entry 'application-tool-operation)
                                     (string= namespace
                                              (tool-namespace
                                               (application-operation-backend entry)))))
                              operations)
                    (application-help--inline-code (format nil "(help ~S)" namespace))))
          (format stream "No callable tools are registered.")))))

(-> application-help--tool-namespaces (list) list)
(defun application-help--tool-namespaces (operations)
  "Return sorted namespaces of the callable tools among OPERATIONS."
  (sort (remove-duplicates
         (loop for operation in operations
               when (typep operation 'application-tool-operation)
                 collect (tool-namespace (application-operation-backend operation)))
         :test #'string=)
        #'string<))


;;;; -- Subject and Text Handling --

(-> application-help--topic-name (t) (option string))
(defun application-help--topic-name (topic)
  "Normalize a bounded symbol or string help subject without interning user text."
  (when (typep topic '(or string symbol))
    (let ((name (string-trim '(#\Space #\Tab #\Newline #\Return) (string topic))))
      (when (and (plusp (length name)) (<= (length name) 200))
        (string-downcase (string-left-trim '(#\: #\/) name))))))

(-> application-help--find-operation (string list) (option application-operation))
(defun application-help--find-operation (name operations)
  "Find a canonical operation or a command alias in one registry snapshot."
  (or (find name operations :test #'string-equal :key #'application-operation-name)
      (find-if (lambda (entry)
                 (and (typep entry 'application-command-operation)
                      (member (concatenate 'string "/" name)
                              (application-command-aliases (application-operation-backend entry))
                              :test #'string-equal)))
               operations)))

(-> application-help--summary (application-operation) string)
(defun application-help--summary (operation)
  "Return the first description line, bounded for a compact operation listing."
  (let* ((description (application-operation-description operation))
         (line        (subseq description 0 (position #\Newline description)))
         (prefix      (text-cell-prefix line 160)))
    (if (= (length prefix) (length line))
        line
        (concatenate 'string prefix "…"))))

(-> application-help--inline-code (string) string)
(defun application-help--inline-code (text)
  "Quote TEXT as a CommonMark code span, including any literal backticks."
  (let ((longest 0)
        (run     0))
    (loop for character across text
          do (if (char= character #\`)
                 (setf longest (max longest (incf run)))
                 (setf run 0)))
    (let ((fence (make-string (1+ longest) :initial-element #\`)))
      (format nil "~A ~A ~A" fence text fence))))
