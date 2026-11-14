(in-package #:autolith)

(defvar *application-lisp-tool-results* nil
  "Evaluation-local EQ map from tool content to immutable display snapshots.
A present NIL entry records ambiguous provenance.")

;;;; -- User-Facing Operation Protocol --

(defclass application-operation ()
  ((name
    :initarg       :name
    :reader        application-operation-name
    :type          non-empty-string
    :documentation "The canonical lowercase Lisp function name.")
   (description
    :initarg       :description
    :reader        application-operation-description
    :type          non-empty-string
    :documentation "The concise user-facing operation description.")
   (backend
    :initarg       :backend
    :reader        application-operation-backend
    :type          t
    :documentation "The exact registered operation backend."))
  (:documentation "One immutable user-facing registered operation."))

(defclass application-command-operation (application-operation)
  ((backend
    :type          application-command
    :documentation "The registered interactive command this operation invokes."))
  (:documentation "A user-facing interactive command operation."))

(defclass application-local-operation (application-operation)
  ((backend
    :type          symbol
    :documentation "The symbol naming the built-in local Lisp operation."))
  (:documentation "A built-in local Lisp operation."))

(defclass application-tool-operation (application-operation)
  ((backend
    :type          tool
    :documentation "The registered tool this operation calls locally."))
  (:documentation "A locally callable tool operation."))

(defgeneric application-operation-kind (operation)
  (:documentation "Return OPERATION's compatibility family keyword."))

(defmethod application-operation-kind ((operation application-command-operation))
  "Identify a command operation through the compatibility protocol."
  (declare (ignore operation))
  ':command)

(defmethod application-operation-kind ((operation application-local-operation))
  "Identify a local operation through the compatibility protocol."
  (declare (ignore operation))
  ':local)

(defmethod application-operation-kind ((operation application-tool-operation))
  "Identify a tool operation through the compatibility protocol."
  (declare (ignore operation))
  ':tool)

(defgeneric application-operation-completion-argument (operation)
  (:documentation "Return OPERATION's completion argument suffix, when present."))

(defgeneric application-operation-active-turn-action (operation arguments)
  (:documentation "Return OPERATION's active-turn admission for native arguments or a parsed command invocation."))

(defgeneric application-operation-immediate-arguments-p (operation argument-forms)
  (:documentation "Return whether OPERATION may inspect ARGUMENT-FORMS immediately."))

(defmethod application-operation-immediate-arguments-p
    ((operation application-tool-operation) argument-forms)
  "Permit computed channel messages at the local user boundary."
  (declare (ignore argument-forms))
  (or (typep (application-operation-backend operation) 'channel-tool)
      (call-next-method)))

(defgeneric application-operation-invoke (operation application arguments &key context)
  (:documentation "Invoke OPERATION with native arguments, a parsed command invocation, or a tool's JSON object and explicit CONTEXT."))

(defgeneric application-operation-binding-eligible-p (operation)
  (:documentation "Return whether OPERATION receives a canonical function binding."))

(define-condition application-operation-loop-action (error)
  ((action
    :initarg       :action
    :reader        application-operation-loop-action-action
    :type          (member :quit)
    :documentation "The application loop action requested by the operation."))
  (:report
   (lambda (condition stream)
     (format stream "The local operation requested ~S."
             (application-operation-loop-action-action condition))))
  (:documentation
   "A local operation requested control transfer to the application loop."))

(defvar *application-operation-application* nil
  "The application whose registered operations are callable during local Lisp.")

(defvar *application-operation-bindings* (make-hash-table :test #'eq)
  "Function bindings installed for canonical operation symbols.")

(defvar *application-local-user-evaluation-p* nil
  "Whether evaluation came from one explicit local Lisp submission.")

(-> read-file ((or string pathname)) string)
(defun read-file (path)
  "Read bounded UTF-8 text from PATH for a local message."
  (with-open-file (stream path :external-format ':utf-8)
    (let ((buffer (make-string 4096))
          (count  0))
      (with-output-to-string (output)
        (loop for size = (read-sequence buffer stream)
              while (plusp size)
              do (incf count size)
                 (when (> count *world-message-text-limit*)
                   (error 'tool-error :tool-name "read-file"
                          :message               "The file exceeds the channel message text limit."))
                 (write-string buffer output :end size))))))

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
   (make-instance 'application-local-operation :name "read-file" :description
                  "Read bounded UTF-8 text for a computed local message."
                  :backend 'read-file)
   (make-instance 'application-local-operation :name "eval-now" :description
                  "Force explicit local Lisp to run during an active turn."
                  :backend 'eval-now)
   (make-instance 'application-local-operation :name "vault-contents"
                  :description "Read or edit the queued input vault." :backend
                  'vault-contents))
  "The built-in local forms advertised beside registered commands and tools.")

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

(defgeneric tool-active-turn-action (tool)
  (:documentation
   "Return :EXECUTE or :HOLD for an explicit local call to TOOL during an active turn."))

(defmethod tool-active-turn-action ((tool channel-tool))
  "Admit channel messages and subscriptions beside an active turn."
  (declare (ignore tool))
  ':execute)

(defmacro define-tool-active-turn-actions (action &body specifications)
  "Define constant ACTION methods for tool classes in SPECIFICATIONS."
  `(progn
     ,@(loop for (class documentation) in specifications
             collect
             `(defmethod tool-active-turn-action ((tool ,class))
                ,documentation
                (declare (ignore tool))
                ,action))))

(define-tool-active-turn-actions :hold
  (tool "Hold unclassified tools at the agent boundary by default.")
  (shell-run-tool "Hold shell execution because authorization may require terminal ownership.")
  (mcp-provider-tool "Hold external MCP calls because approval may require terminal ownership."))

(define-tool-active-turn-actions :execute
  (resource-tool "Permit revision-gated resource operations during an active turn.")
  (workspace-tool "Permit non-modal workspace operations during an active turn.")
  (lisp-tool "Permit isolated worker operations during an active turn.")
  (web-run-tool "Permit provider-backed web searches during an active turn.")
  (papercut-tool "Permit papercut reporting during an active turn.")
  (agenda-tool "Permit workspace-agenda operations during an active turn.")
  (plan-tool "Permit workspace-plan operations during an active turn.")
  (task-orchestrator-tool "Permit lock-protected child and job operations during an active turn.")
  (mcp-managed-tool "Permit non-modal MCP discovery and resource operations during an active turn.")
  (self-status-tool "Permit read-only active-image status inspection during an active turn.")
  (self-diff-tool "Permit read-only active-image mutation inspection during an active turn.")
  (self-generations-tool "Permit read-only retained-generation inspection during an active turn."))


;;;; -- Registry Projection --

(-> application-operation--command-name (application-command) non-empty-string)
(defun application-operation--command-name (command)
  "Return COMMAND's canonical Lisp operation name without the leading slash."
  (subseq (application-command-name command) 1))

(-> application-operation--from-command
    (application-command &key (:name (option string)))
    application-command-operation)
(defun application-operation--from-command (command &key name)
  "Project registered COMMAND into its canonical or alias operation NAME."
  (make-instance 'application-command-operation
                 :name        (or name (application-operation--command-name command))
                 :description (application-command-description command)
                 :backend     command))

(-> application-operation--from-tool (tool) application-tool-operation)
(defun application-operation--from-tool (tool)
  "Project registered TOOL into one user-facing operation."
  (make-instance 'application-tool-operation
                 :name        (tool-canonical-name tool)
                 :description (tool-description tool)
                 :backend     tool))

(-> application-operation--tools (application) list)
(defun application-operation--tools (application)
  "Return APPLICATION's ordered locally callable tools."
  (let ((registry
          (and (slot-boundp application 'tool-registry)
               (application-tool-registry application))))
    (and (typep registry 'tool-registry)
         (remove-if-not #'tool-user-callable-p
                        (tool-registry-tools registry)))))

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

(-> application-operation-list (application &key (:include-aliases-p boolean)) list)
(defun application-operation-list (application &key include-aliases-p)
  "Return APPLICATION's operations, optionally including registered command aliases."
  (application-operation--validate-unique-names
   (append (loop for command in (application-command-list)
                 collect (application-operation--from-command command)
                 when include-aliases-p
                   append (loop for alias in (application-command-aliases command)
                                when (eq command (application-command-find alias))
                                  collect (application-operation--from-command
                                           command :name (subseq alias 1))))
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
          (application-operation-list application :include-aliases-p t)
          :test #'string=
          :key  (lambda (operation)
                  (string-downcase (application-operation-name operation))))))


;;;; -- Completion and Slash Compatibility --

(defparameter *application-operation-property-name-characters* 80
  "The largest JSON property name displayed in one operation completion.")

(-> application-operation--tool-property-names (tool) (values list list))
(defun application-operation--tool-property-names (tool)
  "Return TOOL's displayable required and optional properties in stable order."
  (let* ((parameters     (tool-parameters tool))
         (properties     (and (json-object-p parameters)
                              (json-get parameters "properties")))
         (required-value (and (json-object-p parameters)
                              (json-get parameters "required")))
         (required-order
           (remove-if-not
            #'stringp
            (typecase required-value
              (list
               required-value)
              (vector
               (coerce required-value 'list))
              (t
               nil))))
         (names
           (and (json-object-p properties)
                (sort
                 (loop for name being the hash-keys of properties
                       when (and (non-empty-string-p name)
                                 (<= (length name)
                                     *application-operation-property-name-characters*)
                                 (every #'graphic-char-p name))
                         collect name)
                 #'string<)))
         (required
           (remove-if-not (lambda (name)
                            (member name names :test #'string=))
                          required-order)))
    (values required
            (remove-if (lambda (name)
                         (member name required :test #'string=))
                       names))))

(-> application-operation--tool-argument-fragment (string boolean) string)
(defun application-operation--tool-argument-fragment (name required-p)
  "Return one readable completion fragment for tool property NAME."
  (let ((fragment
          (format nil "~A ~A"
                  (application--lisp-key-token name)
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
            (loop for name in (append required optional)
                  collect
                  (application-operation--tool-argument-fragment
                   name (and (member name required :test #'string=) t)))))
      (and fragments (format nil "~{~A~^ ~})" fragments)))))

(defmethod application-operation-completion-argument
    ((operation application-command-operation))
  "Return a command's native Lisp argument hint with its closing parenthesis."
  (let* ((command (application-operation-backend operation))
         (options (application-command-static-options command))
         (hint
           (if options
               (multiple-value-bind (minimum maximum)
                   (application-command--lambda-list-arity
                    (application-command-lambda-list command))
                 (declare (ignore maximum))
                 (application-command--static-option-hint
                  options (zerop minimum) :native-p t))
               (application-command-argument command))))
    (and hint (format nil "~A)" hint))))

(defmethod application-operation-completion-argument ((operation application-local-operation))
  "Return the generic local-form completion argument."
  (declare (ignore operation))
  "FORM)")

(defmethod application-operation-completion-argument ((operation application-tool-operation))
  "Return a tool's schema-derived keyword argument hint."
  (application-operation--tool-argument-hint
   (application-operation-backend operation)))

(-> application-operation-completion-entry (application-operation) list)
(defun application-operation-completion-entry (operation)
  "Return OPERATION's parenthesized completion, grouping aliases with their command."
  (let* ((name     (application-operation-name operation))
         (argument (application-operation-completion-argument operation))
         (entry    (list :name        (if argument
                                         (format nil "(~A" name)
                                         (format nil "(~A)" name))
                         :argument    argument
                         :description (copy-seq (application-operation-description operation)))))
    (when (typep operation 'application-command-operation)
      (let ((canonical (application-operation--command-name (application-operation-backend operation))))
        (unless (equal name canonical)
          (setf (getf entry :primary) (if argument
                                         (format nil "(~A" canonical)
                                         (format nil "(~A)" canonical))))))
    entry))

(-> application-operation--command-option-completion-entries () list)
(defun application-operation--command-option-completion-entries ()
  "Return readable native Lisp completions for finite command options."
  (let ((*print-case* ':downcase))
    (loop for command in (application-command-list)
          for name = (application-operation--command-name command)
          for primary = (if (application-command-argument command)
                            (format nil "(~A" name)
                            (format nil "(~A)" name))
          append
          (loop for option in (application-command-static-options command)
                collect
                (list :name        (format nil "(~A ~S)" name option)
                      :argument    nil
                      :description (copy-seq (application-command-description command))
                      :primary     primary)))))

(-> application-operation-completion-entries (application) list)
(defun application-operation-completion-entries (application)
  "Return command, Lisp, option, and help-subject completions for APPLICATION."
  (append (application-command-completion-entries)
          (application-command-alias-completion-entries)
          (application-command-option-completion-entries)
          (mapcar #'application-operation-completion-entry
                  (application-operation-list application :include-aliases-p t))
          (application-operation--command-option-completion-entries)
          (application-help-completion-entries application)))

(-> application-path--file-result-paths (string) list)
(defun application-path--file-result-paths (text)
  "Return relative paths from one rendered fff file-search TEXT."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for marker = (search " [" line)
        when (and marker
                  (not (search "indexed; page" line))
                  (not (uiop:string-prefix-p "next-page:" line)))
          collect (subseq line 0 marker)))

(-> application-path--query-match-p (string string) boolean)
(defun application-path--query-match-p (path query)
  "Return true when PATH contains QUERY as a contiguous case-insensitive substring."
  (or (zerop (length query))
      (and (search (string-downcase query) (string-downcase path))
           t)))

(-> application-path-search-files
    (application string &key (:limit (integer 1)))
    list)
(defun application-path-search-files
    (application query &key (limit *terminal-path-completion-limit*))
  "Return ranked workspace-relative paths for QUERY through fff.

fff's own fuzzy matcher is a loose subsequence. Keep its ranking, then drop
paths that do not contain QUERY as a contiguous substring."
  (let* ((registry (application-tool-registry application))
         (tool     (and registry (tool-registry-find registry "search" "files"))))
    (unless (typep tool 'search-tool)
      (return-from application-path-search-files '()))
    (handler-case
        (let ((paths
                (remove-if-not
                 (lambda (path)
                   (application-path--query-match-p path query))
                 (application-path--file-result-paths
                  (search-worker-request
                   (search-tool-engine tool)
                   (application-configuration application)
                   :operation ':files
                   :arguments (list query
                                    :glob-p    nil
                                    :page      0
                                    :page-size *search-maximum-result-limit*))))))
          (subseq paths 0 (min limit (length paths))))
      (error ()
        '()))))

(-> application-path--completion-provider (function &key (:on-ready (option function))) function)
(defun application-path--completion-provider (search-function &key on-ready)
  "Return cached paths immediately, coalescing searches onto one background worker.

Only the latest requested query is searched next. Results for another query are
never returned. Refresh an unchanged query after two seconds without blocking input."
  (let ((lock         (make-lock "path completion"))
        (requested    nil)
        (completed    nil)
        (completed-at 0)
        (paths        nil)
        (running-p    nil))
    (labels ((gather ()
               (loop
                 (let* ((query  (with-lock-held (lock) requested))
                        (result (handler-case (funcall search-function query)
                                  (error () nil))))
                   (when (with-lock-held (lock)
                           (setf completed query
                                 completed-at (get-internal-real-time)
                                 paths result)
                           (when (equal requested query)
                             (setf running-p nil)
                             t))
                     (when on-ready
                       (handler-case (funcall on-ready)
                         (error () nil)))
                     (return))))))
      (lambda (query)
        (with-lock-held (lock)
          (setf requested (copy-seq query))
          (unless (or running-p
                      (and (equal query completed)
                           (< (- (get-internal-real-time) completed-at)
                              (* 2 internal-time-units-per-second))))
            (setf running-p t)
            (handler-case (make-thread #'gather :name "Autolith path completion")
              (error ()
                (setf running-p nil))))
          (when (equal query completed)
            paths))))))

(-> application-operation-connect-ui (application) application)
(defun application-operation-connect-ui (application)
  "Connect APPLICATION's UI to its dynamic per-session operation
 and fff path completions."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (setf (terminal-ui-completion-function ui)
            (lambda ()
              (application-operation-completion-entries application))
            (terminal-ui-path-search-function ui)
            (application-path--completion-provider
             (lambda (query) (application-path-search-files application query))
             :on-ready (lambda ()
                         (with-terminal-ui-locked (ui)
                           (when (terminal-ui-started-p ui)
                             (terminal-ui--repaint-live ui))))))
      (when (slot-boundp application 'configuration)
        (setf (terminal-ui-completion-root ui)
              (config :working-directory
               (application-configuration application))))))
  application)

(defmethod initialize-instance :after ((application application) &key)
  "Connect a newly initialized APPLICATION to operation completion, transcript clicks and status."
  (application-operation-connect-ui application)
  (application-connect-transcript-actions application)
  (application-connect-program-status application))

(-> application-operation-command-hint-form (application-command-invocation) string)
(defun application-operation-command-hint-form (invocation)
  "Return INVOCATION's preferred canonical Lisp spelling."
  (let* ((command   (application-command-invocation-command invocation))
         (name      (application-operation--command-name command))
         (arguments (application-command-invocation-arguments invocation))
         (remainder (application-command-invocation-remainder invocation)))
    (if (application-command-callable-p command)
        (format nil "(~A~{ ~S~})" name arguments)
        (if (non-empty-string-p remainder)
            (format nil "(~A ~S)" name remainder)
            (format nil "(~A)" name)))))

(-> application-operation-present-command-hint
    (application application-command-invocation) null)
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

(define-condition application-operation-value-error (configuration-error portable-value-error)
  ()
  (:default-initargs :message "A local tool argument cannot cross the JSON boundary.")
  (:documentation "A payload-free local JSON argument rejection with a structured reason."))

(-> application-operation--json-key (t) non-empty-string)
(defun application-operation--json-key (key)
  "Return KEY as one JSON object member name.

A string names its member exactly. A symbol the reader upcased, such as
:FILE-PATH, names the lowercase member; one whose name has a lowercase letter,
such as :|filePath|, was escaped on purpose and keeps its case."
  (let ((name
          (typecase key
            (string
             key)
            (symbol
             (let ((name (symbol-name key)))
               (if (some #'lower-case-p name)
                   name
                   (string-downcase name))))
            (t
             (error 'application-operation-value-error :reason ':key)))))
    (unless (non-empty-string-p name)
      (error 'application-operation-value-error :reason ':key))
    name))

(-> application-operation--json-value (t) t)
(defun application-operation--json-value (value)
  "Translate one explicit Lisp VALUE into the project's JSON value convention."
  (handler-case
      (portable-value-map
       value
       (lambda (item child)
         (cond
           ((eq item t)
            t)
           ((or (json-false-p item) (null item))
            (json-false))
           ((eq item ':null)
            nil)
           ((stringp item)
            (copy-seq item))
           ((pathnamep item)
            (namestring item))
           ((characterp item)
            (string item))
           ((or (integerp item) (floatp item))
            item)
           ((symbolp item)
            (string-downcase (symbol-name item)))
           ((hash-table-p item)
            (let ((object (json-object)))
              (maphash
               (lambda (key member)
                 (let ((name (application-operation--json-key key)))
                   (when (nth-value 1 (gethash name object))
                     (error 'application-operation-value-error :reason ':duplicate-key))
                   (setf (gethash name object) (funcall child member))))
               item)
              object))
           ((vectorp item)
            (map 'vector child item))
           (t
            (error 'application-operation-value-error :reason ':value))))
       :containers-p   t
       :proper-lists-p t
       :list           (lambda (items) (coerce items 'vector)))
    (portable-value-error (condition)
      (error 'application-operation-value-error
             :reason (portable-value-error-reason condition)))))

(-> application-operation--tool-arguments (list &optional (option tool)) json-object)
(defun application-operation--tool-arguments (arguments &optional tool)
  "Translate native keyword arguments to JSON, expanding rich channel messages."
  (unless (and (proper-list-p arguments) (evenp (length arguments)))
    (error 'configuration-error :message
           "A local tool call requires alternating keyword and value arguments."))
  (when
      (and (typep tool 'channel-tool) (string= (tool-name tool) "send")
           (typep (getf arguments :text) 'user-message-input))
    (when (nth-value 1 (get-properties arguments '(:images)))
      (error 'tool-error :tool-name "channel.send" :message
             "Rich content already supplies its images."))
    (let ((input (getf arguments :text)))
      (setf arguments (copy-list arguments)
            (getf arguments :text) (user-message-input-text input)
            (getf arguments :images)
            (user-message-input-image-pathnames input))))
  (let ((object (json-object)))
    (loop for (key value) on arguments by (partial #'nthcdr 2)
          for name = (application-operation--json-key key)
          do (when (nth-value 1 (gethash name object))
               (error 'application-operation-value-error :reason
                      ':duplicate-key)) (setf (gethash name object)
                                              (application-operation--json-value
                                               value)))
    object))

(-> application-operation-tool-call-form (string (option json-object)) string)
(defun application-operation-tool-call-form (name arguments)
  "Return the local Lisp call invoking tool NAME with decoded JSON ARGUMENTS.

Evaluating it at the prompt sends the same arguments again. Keys are spelled
as presented call keys, and values as APPLICATION-OPERATION--LISP-VALUE
spells them, in the order the arguments were decoded."
  (with-output-to-string (stream)
    (format stream "(~A" (application--lisp-symbol-token name))
    (when arguments
      (maphash (lambda (key value)
                 (format stream " ~A ~A"
                         (application--lisp-key-token key)
                         (application-operation--lisp-value value)))
               arguments))
    (write-char #\) stream)))

(-> application-operation--lisp-value (t) string)
(defun application-operation--lisp-value (value)
  "Return decoded JSON VALUE as Lisp source evaluating to the same JSON value.

True and false become T and NIL, null becomes :NULL, an array a VECTOR call
so it stays an array when empty, and an object a DICT call."
  (cond
    ((eq value t)
     "t")
    ((json-false-p value)
     "nil")
    ((null value)
     ":null")
    ((hash-table-p value)
     (with-output-to-string (stream)
       (write-string "(dict" stream)
       (maphash (lambda (key member)
                  (format stream " ~S ~A" key (application-operation--lisp-value member)))
                value)
       (write-char #\) stream)))
    ((and (vectorp value) (not (stringp value)))
     (format nil "(vector~{ ~A~})"
             (map 'list #'application-operation--lisp-value value)))
    (t
     (let ((*print-pretty* nil))
       (prin1-to-string value)))))

(-> application-operation--command-invocation
    (application-command (or list application-command-invocation))
    application-command-invocation)
(defun application-operation--command-invocation (command arguments)
  "Return a parsed invocation or preserve evaluated ARGUMENTS for COMMAND.

The command name alone labels Lisp presentation. Never print or traverse
argument objects to construct a label; only the outer argument list is copied."
  (etypecase arguments
    (application-command-invocation
     (unless (eq command (application-command-invocation-command arguments))
       (error 'configuration-error
              :message "The parsed invocation belongs to a different command registration."))
     arguments)
    (list
     (let ((name (application-command-name command)))
       (make-instance 'application-command-invocation
                      :input                   (format nil "(~A)" (application-operation--command-name command))
                      :syntax                  ':lisp
                      :name                    name
                      :remainder               ""
                      :argument                nil
                      :arguments               (copy-list arguments)
                      :supplied-argument-count (length arguments)
                      :command                 command)))))


;;;; -- Active-Turn Admission --

(-> application-operation--immediate-argument-p (t) boolean)
(defun application-operation--immediate-argument-p (form)
  "Return whether FORM is literal data safe to evaluate beside an active turn."
  (or (constantp form)
      (and (proper-list-p form)
           (member (first form) '(list vector json-object) :test #'eq)
           (every #'application-operation--immediate-argument-p (rest form)))))

(defmethod application-operation-active-turn-action
    ((operation application-command-operation) argument-forms)
  "Return command busy policy, retaining modal safety unless explicitly immediate."
  (let* ((command (application-operation-backend operation))
         (invocation
           (application-operation--command-invocation command argument-forms))
         (action  (application-command-busy-action command invocation)))
    ;; Explicitly immediate commands own only presentation or use a coordinated
    ;; world transition. A modal state mutation still waits for its safe boundary.
    (if (and (eq action ':execute)
             (application-command-terminal-owner-p command invocation)
             (not (eq (application-command-busy-behavior command) ':execute)))
        ':hold
        action)))

(defmethod application-operation-active-turn-action
    ((operation application-local-operation) argument-forms)
  "Permit local operations whose own runtime guards enforce their boundaries."
  (declare (ignore operation argument-forms))
  ':execute)

(defmethod application-operation-active-turn-action
    ((operation application-tool-operation) argument-forms)
  "Return the tool backend's active-turn policy."
  (declare (ignore argument-forms))
  (tool-active-turn-action (application-operation-backend operation)))

(defmethod application-operation-immediate-arguments-p
    ((operation application-operation) argument-forms)
  "Require every ordinary operation argument to be immediate literal data."
  (declare (ignore operation))
  (every #'application-operation--immediate-argument-p argument-forms))

(defmethod application-operation-immediate-arguments-p
    ((operation application-local-operation) argument-forms)
  "Permit local operations to apply their own argument evaluation policy."
  (declare (ignore operation argument-forms))
  t)

(-> application-operation-source-active-turn-action
    (application string)
    (member :apply :cancel :execute :hold))
(defun application-operation-source-active-turn-action (application source)
  "Classify one complete explicit Lisp SOURCE beside APPLICATION's active turn.

Only top-level EVAL-NOW, a vault control, or a registered operation with literal
argument forms may run immediately. Arbitrary Lisp and computed operation arguments
wait for the serialized application boundary."
  (handler-case
      (let* ((*package* (find-package '#:autolith))
             (form      (self-read-form source :read-eval nil)))
        (cond
          ((application-operation--immediate-vault-form-p form)
           ':execute)
          ((and (proper-list-p form)
                (symbolp (first form)))
           (let* ((operation (application-operation-find application (first form)))
                  (arguments (rest form)))
             (if (and operation
                      (application-operation-immediate-arguments-p operation arguments))
                 (application-operation-active-turn-action operation arguments)
                 ':hold)))
          (t
           ':hold)))
    (error ()
      ':hold)))

(-> application-operation--vault-place-p (t) boolean)
(defun application-operation--vault-place-p (form)
  "Return whether FORM is a VAULT-CONTENTS place with an optional index."
  (and (proper-list-p form)
       (eq (first form) 'vault-contents)
       (<= (length (rest form)) 1)))

(-> application-operation--immediate-vault-form-p (t) boolean)
(defun application-operation--immediate-vault-form-p (form)
  "Recognize direct vault reads and SETF with exclusively vault places."
  (or (application-operation--vault-place-p form)
      (and (proper-list-p form)
           (eq (first form) 'setf)
           (rest form)
           (evenp (length (rest form)))
           (loop for place in (rest form) by (partial #'nthcdr 2)
                 always (application-operation--vault-place-p place)))))


;;;; -- Authoritative Dispatch --

(-> application-operation--tool-context (application) tool-context)
(defun application-operation--tool-context (application)
  "Return the primary local-user tool context for APPLICATION."
  (make-instance
   'tool-context
   :configuration (application-configuration application)
   :worker        (and (slot-boundp application 'worker)
                       (application-worker application))
   :conversation  (application-conversation application)
   :registry      (application-tool-registry application)
   :agent         (and (slot-boundp application 'agent)
                       (application-agent application))
   :observer
   (callback-agent-observer-create
    :status-callback
    (lambda (status details)
      (when (eq status ':tool-call-progress)
        (let ((activity (getf details :activity)))
          (when (non-empty-string-p activity)
            (application-set-local-activity application activity))))))
   :command-authorization-function
   (lambda (command directory)
     (application-authorize-command application command directory))
   :tool-authorization-function
   (lambda (tool arguments)
     (application-authorize-tool application tool arguments))))

(defmethod application-operation-invoke
    ((operation application-command-operation) application arguments &key context)
  "Invoke a command through its existing handler without duplicating Lisp source."
  (declare (ignore context))
  (let* ((command                                       (application-operation-backend operation))
         (invocation
           (application-operation--command-invocation command arguments))
         (*application-command-presentation-invocation* invocation)
         (*application-command-presentation-pending-p*  nil)
         (action                                        (application-command-execute command application invocation)))
    (case action
      (:continue
       nil)
      (:quit
       (error 'application-operation-loop-action :action ':quit)))))

(defgeneric application-lisp--native-tool-value (result)
  (:documentation
   "Return RESULT's exact native value and presence flag, or NIL and NIL.
Only result types with an explicit native data contract opt in."))

(defmethod application-lisp--native-tool-value ((result tool-result))
  "Use report text when RESULT has no explicit native data contract."
  (declare (ignore result))
  (values nil nil))

(defmethod application-lisp--native-tool-value ((result task-tool-result))
  "Project the exact native task or job form."
  (values (tool-result-details result) t))

(defmethod application-lisp--native-tool-value ((result mission-tool-result))
  "Project the actual mission goal rather than its transcript envelope."
  (multiple-value-bind (key goal)
      (get-properties (rest (tool-result-details result)) '(:goal))
    (values goal (not (null key)))))

(-> application-lisp--snapshot-tool-value
    (tool-result application) list)
(defun application-lisp--snapshot-tool-value (result application)
  "Snapshot a tool report and its content identity guard for local display.
Use explicit native data when available, otherwise retain the report text.
No live result, tool, or native data object escapes in the snapshot."
  (let ((content (copy-seq (tool-result-content result))))
    (or (handler-case
            (multiple-value-bind (value present-p)
                (application-lisp--native-tool-value result)
              (when present-p
                (multiple-value-bind (text printed-p)
                    (application-lisp--render-value
                     value :columns        (application-lisp--value-columns application)
                           :initial-column 2)
                  (when printed-p
                    (list text content ':lisp)))))
          (error () nil))
        (list content content ':text))))

(defmethod application-operation-invoke
    ((operation application-tool-operation) application arguments &key context)
  "Invoke a tool through its decoder, execution method, and caller's explicit capabilities."
  (let* ((tool           (application-operation-backend operation))
         (canonical-name (tool-canonical-name tool))
         (json-arguments (if (hash-table-p arguments)
                             arguments
                             (application-operation--tool-arguments arguments tool)))
         (decoded-arguments
           (tool-decode-arguments tool (json-encode json-arguments))))
    (unless (json-object-p decoded-arguments)
      (error 'tool-error
             :message
             (format nil "Arguments for ~A are not a JSON object." canonical-name)
             :tool-name canonical-name))
    (let* ((ui                (and (slot-boundp application 'ui)
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
                     (or context (application-operation--tool-context application))
                     decoded-arguments))
               (when ui
                 (application-set-local-activity application previous-activity)))))
      (unless (tool-result-success-p result)
        (error 'tool-error
               :message   (tool-failure-summary tool (tool-result-content result))
               :tool-name canonical-name))
      (when *application-lisp-tool-results*
        (let ((content (tool-result-content result)))
          ;; A reused content identity is ambiguous, even for a plain report.
          (setf (gethash content *application-lisp-tool-results*)
                (unless (nth-value 1 (gethash content *application-lisp-tool-results*))
                  (application-lisp--snapshot-tool-value result application)))))
      (tool-result-content result))))

(defmethod application-operation-invoke
    ((operation application-local-operation) application arguments &key context)
  "Reject indirect invocation of a local special form."
  (declare (ignore application arguments context))
  (error 'configuration-error
         :message
         (format nil "Local form ~A must be submitted directly."
                 (application-operation-name operation))))

(-> application-operation-call (application (or string symbol) &rest t) t)
(defun application-operation-call (application identifier &rest arguments)
  "Invoke APPLICATION's registered IDENTIFIER with evaluated Lisp ARGUMENTS."
  (let ((operation (application-operation-find application identifier)))
    (unless operation
      (error 'configuration-error
             :message
             (format nil "No registered user operation is named ~S." identifier)))
    (application-operation-invoke operation application arguments)))


;;;; -- Canonical Function Bindings --

(-> application-operation--function-symbol (non-empty-string) symbol)
(defun application-operation--function-symbol (name)
  "Return the AUTOLITH symbol naming canonical operation NAME."
  (intern (string-upcase name) '#:autolith))

(defmethod application-command-validate-registration ((command application-command))
  "Reject COMMAND when its canonical name or an alias would shadow a function."
  (dolist (name (cons (application-command-name command) (application-command-aliases command)))
    (let* ((symbol    (application-operation--function-symbol (subseq name 1)))
           (installed (gethash symbol *application-operation-bindings*))
           (existing  (and (fboundp symbol) (fdefinition symbol))))
      (when (and existing
                 (not (and installed (eq existing installed))))
        (error 'configuration-error
               :message (format nil "Application command ~A conflicts with existing function ~S."
                                name symbol)))))
  command)

(-> application-operation--binding-function (non-empty-string) function)
(defun application-operation--binding-function (name)
  "Return the dynamic wrapper function for canonical operation NAME."
  (lambda (&rest arguments)
    (unless (typep *application-operation-application* 'application)
      (error 'configuration-error
             :message
             (format nil
                     "Operation ~A requires an active local Autolith evaluation."
                     name)))
    (apply #'application-operation-call
           *application-operation-application* name arguments)))

(-> application-operation--install-binding
    (application-operation)
    (option symbol))
(defun application-operation--install-binding (operation)
  "Install OPERATION's canonical function, or return NIL for a conflicting binding."
  (let* ((name      (application-operation-name operation))
         (symbol    (application-operation--function-symbol name))
         (installed (gethash symbol *application-operation-bindings*))
         (existing  (and (fboundp symbol) (fdefinition symbol))))
    (cond
      ((and installed (eq existing installed))
       symbol)
      ((fboundp symbol)
       nil)
      (t
       (let ((function (application-operation--binding-function name)))
         (setf (symbol-function symbol) function
               (gethash symbol *application-operation-bindings*) function
               (documentation symbol 'function)
               (application-operation-description operation))
         symbol)))))

(defmethod application-operation-binding-eligible-p ((operation application-operation))
  "Permit canonical bindings for ordinary operations."
  (declare (ignore operation))
  t)

(defmethod application-operation-binding-eligible-p
    ((operation application-local-operation))
  "Keep local special forms on their directly defined bindings."
  (declare (ignore operation))
  nil)

(-> application-operation-install-bindings (application) list)
(defun application-operation-install-bindings (application)
  "Install non-conflicting function bindings for canonical operations and aliases."
  (loop for operation in (application-operation-list application :include-aliases-p t)
        for binding = (and (application-operation-binding-eligible-p operation)
                           (application-operation--install-binding operation))
        when binding
          collect binding))
