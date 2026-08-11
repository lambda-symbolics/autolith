(in-package #:autolith)

;;;; -- User-Facing Operation Protocol --

(defclass application-operation ()
  ((name
    :initarg :name
    :reader application-operation-name
    :type non-empty-string
    :documentation "The canonical lowercase Lisp function name.")
   (description
    :initarg :description
    :reader application-operation-description
    :type non-empty-string
    :documentation "The concise user-facing operation description.")
   (kind
    :initarg :kind
    :reader application-operation-kind
    :type (member :command :local :tool)
    :documentation "The authoritative backend family for this operation.")
   (backend
    :initarg :backend
    :reader application-operation-backend
    :type (or symbol application-command tool)
    :documentation "The exact registered command, local form, or tool invoked."))
  (:documentation
   "One immutable user-facing projection of a registered command or tool."))

(define-condition application-operation-loop-action (error)
  ((action
    :initarg :action
    :reader application-operation-loop-action-action
    :type (member :quit)
    :documentation "The application loop action requested by the operation."))
  (:report
   (lambda (condition stream)
     (format stream "The local operation requested ~S."
             (application-operation-loop-action-action condition))))
  (:documentation
   "A local operation requested control transfer to the application loop."))

(defvar *application-operation-application* nil
  "The application whose registered operations are callable during local Lisp.")

(defvar *application-operation-tool-context* nil
  "The tool context whose registered operations are callable in a workflow.")

(defvar *application-operation-bindings* (make-hash-table :test #'eq)
  "Function bindings installed for canonical operation symbols.")

(defvar *application-local-user-evaluation-p* nil
  "Whether evaluation came from one explicit local Lisp submission.")

(define-condition prompt-error (autolith-error)
  ((reason
    :initarg :reason
    :reader prompt-error-reason
    :type keyword
    :documentation "The stable reason prompt admission failed.")
   (target
    :initarg :target
    :initform nil
    :reader prompt-error-target
    :type t
    :documentation "The requested primary or child target, when available."))
  (:documentation "A local PROMPT form could not be validated or admitted."))

(defparameter *prompt-maximum-characters* 131072
  "The largest text accepted by one local PROMPT form.")

(defparameter *prompt-target-maximum-characters* 200
  "The largest child target name accepted by one local PROMPT form.")

(-> prompt--error (keyword string &key (:target t)) nil)
(defun prompt--error (reason message &key target)
  "Signal a typed prompt failure with stable REASON and optional TARGET."
  (error 'prompt-error :message message :reason reason :target target))

(-> prompt--target-name ((or string symbol)) non-empty-string)
(defun prompt--target-name (target)
  "Return TARGET as a bounded case-preserving name or signal PROMPT-ERROR."
  (let ((name (string target)))
    (unless (and (non-empty-string-p name)
                 (<= (length name) *prompt-target-maximum-characters*))
      (prompt--error
       ':invalid-target
       (format nil "PROMPT target must contain from 1 to ~D characters."
               *prompt-target-maximum-characters*)
       :target target))
    (copy-seq name)))

(-> prompt--content (t) non-empty-string)
(defun prompt--content (content)
  "Return validated prompt CONTENT or signal a typed prompt failure."
  (unless (stringp content)
    (prompt--error ':invalid-content "PROMPT content must be a string."))
  (unless (plusp (length content))
    (prompt--error ':empty-content "PROMPT content must not be empty."))
  (when (> (length content) *prompt-maximum-characters*)
    (prompt--error
     ':content-too-large
     (format nil "PROMPT content exceeds the ~D-character limit."
             *prompt-maximum-characters*)))
  (copy-seq content))

(-> application-submit-prompt
    (application (or null string) non-empty-string)
    list)
(defgeneric application-submit-prompt (application target content)
  (:documentation
   "Submit CONTENT to primary when TARGET is NIL, or to child TARGET."))

(-> read-file ((or string pathname)) string)
(defun read-file (path)
  "Read UTF-8 text from PATH without exceeding the local prompt content bound."
  (let ((pathname (pathname path))
        (buffer (make-string 4096))
        (output (make-string-output-stream))
        (characters 0))
    (with-open-file (stream pathname
                            :direction :input
                            :external-format :utf-8)
      (loop for count = (read-sequence buffer stream)
            while (plusp count)
            do (incf characters count)
               (when (> characters *prompt-maximum-characters*)
                 (prompt--error
                  ':content-too-large
                  (format nil "File ~A exceeds the PROMPT content limit."
                          (namestring pathname))
                  :target (namestring pathname)))
               (write-string buffer output :end count)))
    (get-output-stream-string output)))

(-> prompt (&rest t) list)
(defun prompt (&rest arguments)
  "Prompt primary Autolith, or steer a named running child with :TO TARGET."
  (unless (typep *application-operation-application* 'application)
    (prompt--error
     ':no-application
     "PROMPT requires an active local Autolith evaluation."))
  (multiple-value-bind (target content primary-target-p)
      (cond
        ((= (length arguments) 1)
         (values nil (first arguments) t))
        ((and (= (length arguments) 3)
              (eq (first arguments) ':to))
         (values (second arguments) (third arguments) nil))
        (t
         (prompt--error
          ':malformed-arguments
          "Use (prompt CONTENT) or (prompt :to TARGET CONTENT).")))
    (unless (or primary-target-p
                (typep target '(or string symbol)))
      (prompt--error
       ':invalid-target
       "PROMPT target must be a symbol or string."
       :target target))
    (let ((validated-content (prompt--content content))
          (target-name
            (and (not primary-target-p)
                 (prompt--target-name target))))
      (application-submit-prompt
       *application-operation-application* target-name validated-content))))


(defmacro eval-now (&body body)
  "Evaluate BODY as explicit local input even while an agent turn is active.

Only a top-level local submission receives immediate active-turn admission. The
runtime guard prevents provider and ordinary internal code from using this
local-user override."
  `(progn
     (unless *application-local-user-evaluation-p*
       (error 'configuration-error
              :message
              "EVAL-NOW is available only inside explicit local Lisp input."))
     ,@body))

(defparameter *application-local-operations*
  (list
   (make-instance
    'application-operation
    :name "prompt"
    :description "Prompt primary Autolith or steer a named running child."
    :kind ':local
    :backend 'prompt)
   (make-instance
    'application-operation
    :name "read-file"
    :description "Read bounded UTF-8 text for a computed local prompt."
    :kind ':local
    :backend 'read-file)
   (make-instance
    'application-operation
    :name "eval-now"
    :description "Force explicit local Lisp to run during an active turn."
    :kind ':local
    :backend 'eval-now))
  "The built-in local forms advertised beside registered commands and tools.")

(-> tool-user-callable-p (tool) boolean)
(defgeneric tool-user-callable-p (tool)
  (:documentation "Return whether TOOL belongs to the local user's operation surface."))

(defmethod tool-user-callable-p ((tool tool))
  "Expose ordinary primary-session tools to explicit local user calls."
  (declare (ignore tool))
  t)

(defmethod tool-user-callable-p ((tool task-yield-tool))
  "Hide the child-only terminal yield operation from the primary user surface."
  (declare (ignore tool))
  nil)

(-> tool-active-turn-action (tool) (member :execute :hold))
(defgeneric tool-active-turn-action (tool)
  (:documentation
   "Return whether an explicit local call to TOOL may overlap an active turn."))

(defmethod tool-active-turn-action ((tool tool))
  "Hold unclassified tools at the agent boundary by default."
  (declare (ignore tool))
  ':hold)

(defmethod tool-active-turn-action ((tool resource-tool))
  "Permit revision-gated resource operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool workspace-tool))
  "Permit non-modal workspace operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool shell-run-tool))
  "Hold shell execution because authorization may require terminal ownership."
  (declare (ignore tool))
  ':hold)

(defmethod tool-active-turn-action ((tool lisp-tool))
  "Permit isolated worker operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool web-run-tool))
  "Permit provider-backed web searches during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool memory-tool))
  "Permit persistent-memory operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool papercut-tool))
  "Permit papercut reporting during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool agenda-tool))
  "Permit workspace-agenda operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool plan-tool))
  "Permit workspace-plan operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool task-orchestrator-tool))
  "Permit lock-protected child and job operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool mcp-managed-tool))
  "Permit non-modal MCP discovery and resource operations during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool mcp-provider-tool))
  "Hold external MCP calls because approval may require terminal ownership."
  (declare (ignore tool))
  ':hold)

(defmethod tool-active-turn-action ((tool self-inspect-tool))
  "Permit read-only active-image inspection during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool self-source-tool))
  "Permit read-only active-image source lookup during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool self-status-tool))
  "Permit read-only active-image status inspection during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool self-diff-tool))
  "Permit read-only active-image mutation inspection during an active turn."
  (declare (ignore tool))
  ':execute)

(defmethod tool-active-turn-action ((tool self-generations-tool))
  "Permit read-only retained-generation inspection during an active turn."
  (declare (ignore tool))
  ':execute)


;;;; -- Registry Projection --

(-> application-operation--command-name (application-command) non-empty-string)
(defun application-operation--command-name (command)
  "Return COMMAND's canonical Lisp operation name without the leading slash."
  (subseq (application-command-name command) 1))

(-> application-operation--from-command
    (application-command)
    application-operation)
(defun application-operation--from-command (command)
  "Project registered COMMAND into one user-facing operation."
  (make-instance 'application-operation
                 :name (application-operation--command-name command)
                 :description (application-command-description command)
                 :kind ':command
                 :backend command))

(-> application-operation--from-tool (tool) application-operation)
(defun application-operation--from-tool (tool)
  "Project registered TOOL into one user-facing operation."
  (make-instance 'application-operation
                 :name (tool-canonical-name tool)
                 :description (tool-description tool)
                 :kind ':tool
                 :backend tool))

(-> application-operation--tools (application) list)
(defun application-operation--tools (application)
  "Return APPLICATION's ordered locally callable tools."
  (let ((registry
          (and (slot-boundp application 'tool-registry)
               (application-tool-registry application))))
    (if (typep registry 'tool-registry)
        (remove-if-not #'tool-user-callable-p
                       (copy-list (tool-registry-tools registry)))
        nil)))

(-> application-operation--validate-unique-names (list) list)
(defun application-operation--validate-unique-names (operations)
  "Return OPERATIONS after rejecting a duplicate canonical name."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (operation operations)
      (let ((name (string-downcase (application-operation-name operation))))
        (when (gethash name seen)
          (error 'configuration-error
                 :message
                 (format nil
                         "Registered user operations repeat canonical name ~S."
                         name)))
        (setf (gethash name seen) t)))
    operations))

(-> application-operation-list (application) list)
(defun application-operation-list (application)
  "Return APPLICATION's commands, local forms, and callable tools in order."
  (application-operation--validate-unique-names
   (append (mapcar #'application-operation--from-command
                   (application-command-list))
           *application-local-operations*
           (mapcar #'application-operation--from-tool
                   (application-operation--tools application)))))

(-> application-operation-find
    (application (or string symbol))
    (option application-operation))
(defun application-operation-find (application identifier)
  "Return APPLICATION's operation named by case-insensitive IDENTIFIER."
  (let ((name (string-downcase (string identifier))))
    (find name
          (application-operation-list application)
          :test #'string=
          :key (lambda (operation)
                 (string-downcase (application-operation-name operation))))))


;;;; -- Completion and Slash Compatibility --

(defparameter *application-operation-property-name-characters* 80
  "The largest JSON property name displayed in one operation completion.")

(-> application-operation--displayable-property-name-p (t) boolean)
(defun application-operation--displayable-property-name-p (value)
  "Return whether VALUE is a bounded non-empty string safe for one completion row."
  (and (non-empty-string-p value)
       (<= (length value) *application-operation-property-name-characters*)
       (every #'graphic-char-p value)))

(-> application-operation--tool-property-names (tool) (values list list))
(defun application-operation--tool-property-names (tool)
  "Return TOOL's displayable required and optional properties in stable order."
  (let* ((parameters (tool-parameters tool))
         (properties (and (json-object-p parameters)
                          (json-get parameters "properties")))
         (required-value (and (json-object-p parameters)
                              (json-get parameters "required")))
         (property-names
           (if (json-object-p properties)
               (sort (loop for name being the hash-keys of properties
                           when (application-operation--displayable-property-name-p
                                 name)
                             collect name)
                     #'string<)
               nil))
         (required-order
           (remove-if-not
            #'stringp
            (typecase required-value
              (null nil)
              (list required-value)
              (vector (coerce required-value 'list))
              (t nil))))
         (required
           (remove-if-not (lambda (name)
                            (member name property-names :test #'string=))
                          required-order))
         (optional
           (remove-if (lambda (name)
                        (member name required :test #'string=))
                      property-names)))
    (values required optional)))

(-> application-operation--tool-property-keyword (string) string)
(defun application-operation--tool-property-keyword (name)
  "Return property NAME as one reader-safe Lisp keyword token."
  (application--lisp-key-token name))

(-> application-operation--tool-argument-fragment (string boolean) string)
(defun application-operation--tool-argument-fragment (name required-p)
  "Return one readable completion fragment for tool property NAME."
  (let ((fragment
          (format nil
                  "~A ~A"
                  (application-operation--tool-property-keyword name)
                  (if (application--lisp-simple-symbol-name-p name)
                      (string-upcase name)
                      "VALUE"))))
    (if required-p fragment (format nil "[~A]" fragment))))

(-> application-operation--tool-argument-hint (tool) (option string))
(defun application-operation--tool-argument-hint (tool)
  "Return TOOL's Lisp keyword argument hint, including the closing parenthesis."
  (multiple-value-bind (required optional)
      (application-operation--tool-property-names tool)
    (let ((fragments
            (append
             (mapcar (lambda (name)
                       (application-operation--tool-argument-fragment name t))
                     required)
             (mapcar (lambda (name)
                       (application-operation--tool-argument-fragment name nil))
                     optional))))
      (when fragments
        (format nil "~{~A~^ ~})" fragments)))))

(-> application-operation-completion-entry (application-operation) list)
(defun application-operation-completion-entry (operation)
  "Return OPERATION's canonical parenthesized completion entry."
  (let* ((name (application-operation-name operation))
         (backend (application-operation-backend operation))
         (argument
           (ecase (application-operation-kind operation)
             (:command
              (let ((hint (application-command-argument backend)))
                (and hint (format nil "~A)" hint))))
             (:local
              "FORM)")
             (:tool
              (application-operation--tool-argument-hint backend)))))
    (list :name (if argument
                    (format nil "(~A" name)
                    (format nil "(~A)" name))
          :argument argument
          :description (copy-seq (application-operation-description operation)))))

(-> application-operation-completion-entries (application) list)
(defun application-operation-completion-entries (application)
  "Return slash command and canonical Lisp operation completions for APPLICATION."
  (append (application-command-completion-entries)
          (mapcar #'application-operation-completion-entry
                  (application-operation-list application))))

(-> application-operation--help-group (string list) string)
(defun application-operation--help-group (title operations)
  "Return one aligned TITLE section for ordered OPERATIONS."
  (let* ((entries (mapcar #'application-operation-completion-entry operations))
         (label-width
           (loop for entry in entries
                 maximize (length (terminal-completion-label entry)))))
    (format nil
            "~A~%~{~A~^~%~}"
            title
            (loop for entry in entries
                  collect
                  (format nil "~vA  ~A"
                          label-width
                          (terminal-completion-label entry)
                          (getf entry :description))))))

(-> application-operation-help (application) string)
(defun application-operation-help (application)
  "Return APPLICATION's canonical local, command, and tool operation reference."
  (let* ((operations (application-operation-list application))
         (commands (remove-if-not
                    (lambda (operation)
                      (eq (application-operation-kind operation) ':command))
                    operations))
         (locals (remove-if-not
                  (lambda (operation)
                    (eq (application-operation-kind operation) ':local))
                  operations))
         (tools (remove-if-not
                 (lambda (operation)
                   (eq (application-operation-kind operation) ':tool))
                 operations)))
    (format nil
            "Registered operations~%~%~A~%~%~A~%~%~A~%~%Slash commands remain compatibility spellings."
            (application-operation--help-group "Commands" commands)
            (application-operation--help-group "Local evaluation" locals)
            (application-operation--help-group "Tools" tools))))

(-> application-operation-connect-ui (application) application)
(defun application-operation-connect-ui (application)
  "Connect APPLICATION's UI to its dynamic per-session operation completions."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (setf (terminal-ui-completion-function ui)
            (lambda ()
              (application-operation-completion-entries application)))))
  application)

(defmethod initialize-instance :after ((application application) &key)
  "Connect a newly initialized APPLICATION to dynamic operation completion."
  (application-operation-connect-ui application))

(-> application-operation-command-hint-form
    (application-command-invocation)
    string)
(defun application-operation-command-hint-form (invocation)
  "Return INVOCATION's preferred canonical Lisp spelling."
  (let* ((command (application-command-invocation-command invocation))
         (name (application-operation--command-name command))
         (remainder (application-command-invocation-remainder invocation)))
    (if (non-empty-string-p remainder)
        (format nil "(~A ~S)" name remainder)
        (format nil "(~A)" name))))

(-> application-operation-present-command-hint
    (application application-command-invocation)
    null)
(defun application-operation-present-command-hint (application invocation)
  "Present INVOCATION's canonical Lisp spelling once per command and session."
  (let ((command (application-command-invocation-command invocation)))
    (when command
      (let ((name (application-operation--command-name command)))
        (with-recursive-lock-held ((application-render-lock application))
          (unless (member name
                          (application-lisp-hinted-command-names application)
                          :test #'string=)
            (push name (application-lisp-hinted-command-names application))
            (application-present
             application
             (list (terminal-span ':hint "Prefer ")
                   (terminal-span
                    ':code
                    (application-operation-command-hint-form invocation))
                   (terminal-span ':hint "."))))))))
  nil)


;;;; -- Lisp Argument Boundary --

(-> application-operation--proper-list-p (t) boolean)
(defun application-operation--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case
      (and (listp value) (integerp (list-length value)))
    (type-error ()
      nil)))

(-> application-operation--json-key (t) non-empty-string)
(defun application-operation--json-key (key)
  "Return KEY as one lowercase JSON object member name."
  (let ((name
          (etypecase key
            (string key)
            (symbol (symbol-name key)))))
    (unless (non-empty-string-p name)
      (error 'configuration-error
             :message "A local tool argument key must not be empty."))
    (string-downcase name)))

(-> application-operation--json-value (t) t)
(defun application-operation--json-value (value)
  "Translate one explicit Lisp VALUE into the project's JSON value convention."
  (cond
    ((eq value t)
     t)
    ((eq value false)
     false)
    ((null value)
     false)
    ((eq value ':null)
     nil)
    ((stringp value)
     (copy-seq value))
    ((pathnamep value)
     (namestring value))
    ((characterp value)
     (string value))
    ((or (integerp value) (floatp value))
     value)
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ((symbolp value)
     (string-downcase (symbol-name value)))
    ((hash-table-p value)
     (let ((object (json-object)))
       (maphash
        (lambda (key member)
          (let ((name (application-operation--json-key key)))
            (when (nth-value 1 (gethash name object))
              (error 'configuration-error
                     :message
                     (format nil "A local JSON object repeats key ~S." name)))
            (setf (gethash name object)
                  (application-operation--json-value member))))
        value)
       object))
    ((vectorp value)
     (map 'vector #'application-operation--json-value value))
    ((application-operation--proper-list-p value)
     (map 'vector #'application-operation--json-value value))
    (t
     (error 'configuration-error
            :message
            (format nil
                    "Local tool argument value ~S cannot cross the JSON boundary."
                    value)))))

(-> application-operation--tool-arguments (list) json-object)
(defun application-operation--tool-arguments (arguments)
  "Translate alternating Lisp keyword ARGUMENTS into one JSON object."
  (unless (and (application-operation--proper-list-p arguments)
               (evenp (length arguments)))
    (error 'configuration-error
           :message
           "A local tool call requires alternating keyword and value arguments."))
  (let ((object (json-object)))
    (loop for (key value) on arguments by #'cddr
          for name = (application-operation--json-key key)
          do
             (when (nth-value 1 (gethash name object))
               (error 'configuration-error
                      :message
                      (format nil "A local tool call repeats argument ~S." name)))
             (setf (gethash name object)
                   (application-operation--json-value value)))
    object))

(-> application-operation--command-value (t) string)
(defun application-operation--command-value (value)
  "Return one evaluated Lisp VALUE as compatibility command argument text."
  (etypecase value
    (string (copy-seq value))
    (pathname (namestring value))
    (character (string value))
    (symbol (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(-> application-operation--command-invocation
    (application-command list)
    application-command-invocation)
(defun application-operation--command-invocation (command arguments)
  "Return COMMAND's canonical compatibility invocation for evaluated ARGUMENTS."
  (let* ((name (application-command-name command))
         (remainder
           (string-trim
            *application-command-whitespace*
            (format nil
                    "~{~A~^ ~}"
                    (mapcar #'application-operation--command-value arguments))))
         (input
           (if (non-empty-string-p remainder)
               (format nil "~A ~A" name remainder)
               name)))
    (make-instance 'application-command-invocation
                   :input input
                   :name name
                   :remainder remainder
                   :argument (application-command--first-token remainder)
                   :arguments (copy-list arguments)
                   :supplied-argument-count (length arguments)
                   :command command)))


;;;; -- Active-Turn Admission --

(-> application-operation--immediate-argument-p (t) boolean)
(defun application-operation--immediate-argument-p (form)
  "Return whether FORM is literal data safe to evaluate beside an active turn."
  (or (constantp form)
      (and (application-operation--proper-list-p form)
           (member (first form) '(list vector json-object) :test #'eq)
           (every #'application-operation--immediate-argument-p (rest form)))))

(-> application-operation--command-active-turn-action
    (application-command list)
    (member :cancel :execute :hold))
(defun application-operation--command-active-turn-action (command argument-forms)
  "Return COMMAND's existing busy action for literal ARGUMENT-FORMS."
  (let* ((invocation
           (application-operation--command-invocation command argument-forms))
         (action (application-command-busy-action command invocation)))
    (if (and (eq action ':execute)
             (application-command-terminal-owner-p command invocation))
        ':hold
        action)))

(-> application-operation--active-turn-action
    (application-operation list)
    (member :cancel :execute :hold))
(defun application-operation--active-turn-action (operation argument-forms)
  "Return OPERATION's active-turn admission decision for ARGUMENT-FORMS."
  (ecase (application-operation-kind operation)
    (:command
     (application-operation--command-active-turn-action
      (application-operation-backend operation) argument-forms))
    (:local
     ':execute)
    (:tool
     (tool-active-turn-action (application-operation-backend operation)))))

(-> application-operation-source-active-turn-action
    (application string)
    (member :cancel :execute :hold))
(defun application-operation-source-active-turn-action (application source)
  "Classify one complete explicit Lisp SOURCE beside APPLICATION's active turn.

Only top-level EVAL-NOW or a registered operation with literal argument forms may
run immediately. Arbitrary Lisp and computed operation arguments wait for the
serialized application boundary."
  (handler-case
      (let* ((*package* (find-package '#:autolith))
             (form (self-read-form source :read-eval nil)))
        (unless (and (application-operation--proper-list-p form)
                     (symbolp (first form)))
          (return-from application-operation-source-active-turn-action ':hold))
        (let* ((operation (application-operation-find application (first form)))
               (arguments (rest form)))
          (if (and operation
                   (or (eq (application-operation-kind operation) ':local)
                       (every #'application-operation--immediate-argument-p
                              arguments)))
              (application-operation--active-turn-action operation arguments)
              ':hold)))
    (error ()
      ':hold)))


;;;; -- Authoritative Dispatch --

(-> application-operation--tool-context (application) tool-context)
(defun application-operation--tool-context (application)
  "Return the primary local-user tool context for APPLICATION."
  (make-instance
   'tool-context
   :configuration (application-configuration application)
   :worker (and (slot-boundp application 'worker)
                (application-worker application))
   :conversation (application-conversation application)
   :registry (application-tool-registry application)
   :agent (and (slot-boundp application 'agent)
               (application-agent application))
   :command-authorization-function
   (lambda (command directory)
     (application-authorize-command application command directory))
   :tool-authorization-function
   (lambda (tool arguments)
     (application-authorize-tool application tool arguments))))

(-> application-operation--call-command
    (application application-command list)
    null)
(defun application-operation--call-command (application command arguments)
  "Invoke COMMAND through its existing handler without duplicating the Lisp source."
  (let* ((invocation
           (application-operation--command-invocation command arguments))
         (*application-command-presentation-invocation* invocation)
         (*application-command-presentation-pending-p* nil)
         (action (application-command-execute command application invocation)))
    (case action
      (:continue
       nil)
      (:quit
       (error 'application-operation-loop-action :action ':quit)))))

(-> application-operation--call-tool (application tool list) string)
(defun application-operation--call-tool (application tool arguments)
  "Invoke TOOL through its existing decoder and execution method for APPLICATION."
  (let* ((canonical-name (tool-canonical-name tool))
         (json-arguments (application-operation--tool-arguments arguments))
         (decoded-arguments
           (tool-decode-arguments tool (json-encode json-arguments))))
    (unless (json-object-p decoded-arguments)
      (error 'tool-error
             :message
             (format nil "Arguments for ~A are not a JSON object." canonical-name)
             :tool-name canonical-name))
    (let* ((ui (and (slot-boundp application 'ui)
                    (application-ui application)))
           (previous-activity (and ui (terminal-ui-local-activity ui)))
           (result
             (unwind-protect
                  (progn
                    (when ui
                      (application-set-local-activity
                       application (format nil "running ~A" canonical-name)))
                    (tool-execute
                     tool
                     (application-operation--tool-context application)
                     decoded-arguments))
               (when ui
                 (application-set-local-activity
                  application previous-activity)))))
      (unless (tool-result-success-p result)
        (error 'tool-error
               :message (tool-result-content result)
               :tool-name canonical-name))
      (tool-result-content result))))

(-> application-operation--call-context-tool
    (tool-context tool list)
    string)
(defun application-operation--call-context-tool (context tool arguments)
  "Invoke TOOL with evaluated Lisp ARGUMENTS through existing CONTEXT."
  (let* ((canonical-name (tool-canonical-name tool))
         (json-arguments (application-operation--tool-arguments arguments))
         (decoded-arguments
           (tool-decode-arguments tool (json-encode json-arguments))))
    (unless (json-object-p decoded-arguments)
      (error 'tool-error
             :message
             (format nil "Arguments for ~A are not a JSON object."
                     canonical-name)
             :tool-name canonical-name))
    (let ((result (tool-execute tool context decoded-arguments)))
      (unless (tool-result-success-p result)
        (error 'tool-error
               :message (tool-result-content result)
               :tool-name canonical-name))
      (tool-result-content result))))

(-> application-operation-call-context
    (tool-context (or string symbol) &rest t)
    string)
(defun application-operation-call-context (context identifier &rest arguments)
  "Invoke CONTEXT's user-callable tool named IDENTIFIER."
  (let* ((registry (tool-context-registry context))
         (name (string-downcase (string identifier)))
         (tool
           (find name
                 (tool-registry-tools registry)
                 :test #'string=
                 :key #'tool-canonical-name)))
    (unless (and tool (tool-user-callable-p tool))
      (error 'configuration-error
             :message
             (format nil "No workflow-callable tool is named ~S." identifier)))
    (application-operation--call-context-tool context tool arguments)))

(-> application-operation-call
    (application (or string symbol) &rest t)
    t)
(defun application-operation-call (application identifier &rest arguments)
  "Invoke APPLICATION's registered IDENTIFIER with evaluated Lisp ARGUMENTS."
  (let ((operation (application-operation-find application identifier)))
    (unless operation
      (error 'configuration-error
             :message
             (format nil "No registered user operation is named ~S." identifier)))
    (ecase (application-operation-kind operation)
      (:command
       (application-operation--call-command
        application (application-operation-backend operation) arguments))
      (:local
       (error 'configuration-error
              :message
              (format nil "Local form ~A must be submitted directly."
                      (application-operation-name operation))))
      (:tool
       (application-operation--call-tool
        application (application-operation-backend operation) arguments)))))


;;;; -- Canonical Function Bindings --

(-> application-operation--function-symbol (non-empty-string) symbol)
(defun application-operation--function-symbol (name)
  "Return the AUTOLITH symbol naming canonical operation NAME."
  (intern (string-upcase name) '#:autolith))

(-> application-operation--binding-function (non-empty-string) function)
(defun application-operation--binding-function (name)
  "Return the dynamic wrapper function for canonical operation NAME."
  (lambda (&rest arguments)
    (unless (or (typep *application-operation-application* 'application)
                (typep *application-operation-tool-context* 'tool-context))
      (error 'configuration-error
             :message
             (format nil
                     "Operation ~A requires an active local evaluation or Skill workflow."
                     name)))
    (if (typep *application-operation-application* 'application)
        (apply #'application-operation-call
               *application-operation-application* name arguments)
        (apply #'application-operation-call-context
               *application-operation-tool-context* name arguments))))

(-> application-operation--install-binding (application-operation) symbol)
(defun application-operation--install-binding (operation)
  "Install OPERATION's canonical function unless an unrelated binding conflicts."
  (let* ((name (application-operation-name operation))
         (symbol (application-operation--function-symbol name))
         (installed (gethash symbol *application-operation-bindings*))
         (existing (and (fboundp symbol) (fdefinition symbol))))
    (cond
      ((and installed (eq existing installed))
       symbol)
      ((fboundp symbol)
       (error 'configuration-error
              :message
              (format nil
                      "Registered operation ~A conflicts with existing function ~S."
                      name symbol)))
      (t
       (let ((function (application-operation--binding-function name)))
         (setf (symbol-function symbol) function
               (gethash symbol *application-operation-bindings*) function
               (documentation symbol 'function)
               (application-operation-description operation))
         symbol)))))

(-> application-operation-install-bindings (application) list)
(defun application-operation-install-bindings (application)
  "Install canonical function bindings for APPLICATION's commands and tools."
  (loop for operation in (application-operation-list application)
        unless (eq (application-operation-kind operation) ':local)
          collect (application-operation--install-binding operation)))

(-> application-operation-install-context-bindings (tool-context) list)
(defun application-operation-install-context-bindings (context)
  "Install canonical Lisp bindings for CONTEXT's user-callable tools."
  (loop for tool in (tool-registry-tools (tool-context-registry context))
        when (tool-user-callable-p tool)
          collect
          (application-operation--install-binding
           (application-operation--from-tool tool))))
