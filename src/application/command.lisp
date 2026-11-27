(in-package #:autolith)

;;;; -- Interactive Command Protocol --

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *application-help-sections*
    '((:topic :channels :title "Channels"
       :description "Move between channels, message them, and place their agents.")
      (:topic :conversations :title "Conversations"
       :description "Manage the conversation of this channel.")
      (:topic :models :title "Models"
       :description "Choose a provider, model, and reasoning effort.")
      (:topic :workspace :title "Workspace"
       :description "Change directories, permissions, skills, and workspace notes.")
      (:topic :display :title "Display"
       :description "Adjust settings, reasoning, timestamps, and transcript detail.")
      (:topic :runtime :title "Runtime and recovery"
       :description "Inspect the session, save a generation, or recover queued input."))
    "Supported command help topics in editorial presentation order.")

  (defparameter *application-command-metadata-keys*
    '(:name :aliases :argument :description :tip
      :busy-behavior :terminal-behavior :static-options
      :help-topic :help-order)
    "The literal metadata keys accepted by DEFINE-APPLICATION-COMMAND.")

  (defparameter *application-command-required-metadata-keys*
    '(:name :description :tip :busy-behavior :terminal-behavior)
    "The command metadata keys every defining form must state explicitly.")

  (defparameter *application-command-busy-behaviors*
    '(:hold :inspect :execute :apply :apply-interactive)
    "The supported command policies while application work is active.")

  (defparameter *application-command-terminal-behaviors*
    '(:shared :exclusive :exclusive-without-arguments)
    "The supported command policies for terminal reader ownership.")

  (-> application-command--definition-error (t string &rest t) nil)
  (defun application-command--definition-error (command control &rest arguments)
    "Signal an invalid definition of COMMAND, described by CONTROL and ARGUMENTS."
    (error 'application-command-definition-error
           :command command
           :message (apply #'format nil control arguments)))

  (-> application-command--identifier-p (t) boolean)
  (defun application-command--identifier-p (value)
    "Return true when VALUE is a normalized native command identifier."
    (and (non-empty-string-p value)
         (string= value (string-downcase value))
         (not (find-if (lambda (character)
                         (find character '(#\/ #\Space #\Tab #\Newline #\Return #\Page)))
                       value))))

  (-> application-command--metadata-key-count (list symbol) (integer 0))
  (defun application-command--metadata-key-count (metadata key)
    "Return the number of KEY occurrences in literal METADATA."
    (loop for tail on metadata by (partial #'nthcdr 2)
          count (eq (first tail) key)))

  (-> application-command--option-text ((or boolean string keyword)) string)
  (defun application-command--option-text (option)
    "Return the compact display spelling of a native OPTION."
    (etypecase option
      (boolean
       (if option "on" "off"))
      (string
       option)
      (keyword
       (string-downcase (symbol-name option)))))

  (-> application-command--static-options-p (t) boolean)
  (defun application-command--static-options-p (value)
    "Return true for unique printable string, keyword, or boolean choices."
    (and (proper-list-p value)
         (not (null value))
         (every (lambda (option)
                  (and (or (non-empty-string-p option)
                           (keywordp option)
                           (typep option 'boolean))
                       (non-empty-string-p (application-command--option-text option))
                       (every #'graphic-char-p (application-command--option-text option))))
                value)
         (= (length value)
            (length (remove-duplicates value :test #'string=
                                             :key  #'application-command--option-text)))))

  (-> application-command--lambda-list-arity (list)
      (values (integer 0) (option (integer 0))))
  (defun application-command--lambda-list-arity (lambda-list)
    "Return minimum and maximum argument counts.
The maximum is NIL for &REST or &KEY."
    (let ((minimum 0)
          (maximum 0)
          (state   ':required)
          (rest-p  nil))
      (dolist (entry lambda-list)
        (if (and (symbolp entry)
                 (member entry lambda-list-keywords :test #'eq))
            (case entry
              (&optional
               (setf state ':optional))
              (&rest
               (setf state ':rest
                     rest-p t))
              (&key
               (setf state ':key
                     rest-p t))
              (&allow-other-keys
               (unless (eq state ':key)
                 (application-command--definition-error nil "&ALLOW-OTHER-KEYS requires &KEY.")))
              (&aux
               (setf state ':aux))
              ((&whole &environment)
               (application-command--definition-error
                nil "Application commands require ordinary lambda lists."))
              (otherwise
               (application-command--definition-error
                nil "Unsupported application command lambda-list keyword ~S." entry)))
            (case state
              (:required
               (unless (symbolp entry)
                 (application-command--definition-error
                  nil "Required application command parameters must be symbols."))
               (incf minimum)
               (incf maximum))
              (:optional
               (unless (or (symbolp entry)
                           (and (proper-list-p entry)
                                (<= 1 (length entry) 3)
                                (symbolp (first entry))))
                 (application-command--definition-error
                  nil "Invalid optional application command parameter ~S." entry))
               (incf maximum))
              (:rest
               (unless (symbolp entry)
                 (application-command--definition-error
                  nil "An application command rest parameter must be a symbol."))
               (setf state ':after-rest))
              (:key
               (unless (or (symbolp entry)
                           (and (proper-list-p entry)
                                (<= 1 (length entry) 3)
                                (or (symbolp (first entry))
                                    (and (proper-list-p (first entry))
                                         (= 2 (length (first entry)))
                                         (every #'symbolp (first entry))))))
                 (application-command--definition-error
                  nil "Invalid keyword application command parameter ~S." entry)))
              (:after-rest
               (application-command--definition-error
                nil "Unexpected parameter after an application command rest parameter."))
              (:aux
               nil))))
      (when (eq state ':rest)
        (application-command--definition-error
         nil "An application command rest marker requires a parameter."))
      (values minimum (and (not rest-p) maximum))))

  (-> application-command--static-option-hint (list boolean) string)
  (defun application-command--static-option-hint (options optional-p)
    "Return the finite OPTIONS hint in native Lisp syntax."
    (let* ((*print-case* ':downcase)
           (choices      (format nil "~{~S~^|~}" options)))
      (if optional-p (format nil "[~A]" choices) choices)))

  (-> application-command--validate-metadata
      (t &key (:aliases t) (:argument t) (:description t) (:tip t) (:busy-behavior t)
           (:terminal-behavior t) (:static-options t) (:help-topic t) (:help-order t))
      boolean)
  (defun application-command--validate-metadata
      (name &key aliases argument description tip busy-behavior terminal-behavior
                   static-options help-topic help-order)
    "Validate command metadata values and return true."
    (unless (application-command--identifier-p name)
      (application-command--definition-error
       name "Application command name ~S is not a lowercase native identifier." name))
    (unless (proper-list-p aliases)
      (application-command--definition-error
       name "Application command ~A aliases are not a proper literal list." name))
    (unless (every #'application-command--identifier-p aliases)
      (application-command--definition-error
       name "Application command ~A has an invalid alias." name))
    (when (member name aliases :test #'string=)
      (application-command--definition-error
       name "Application command ~A repeats its canonical name as an alias." name))
    (unless (= (length aliases)
               (length (remove-duplicates aliases :test #'string=)))
      (application-command--definition-error name "Application command ~A repeats an alias." name))
    (unless (or (null argument) (non-empty-string-p argument))
      (application-command--definition-error
       name "Application command ~A has an invalid argument hint." name))
    (unless (or (null static-options)
                (application-command--static-options-p static-options))
      (application-command--definition-error
       name "Application command ~A has invalid static options." name))
    (unless (non-empty-string-p description)
      (application-command--definition-error
       name "Application command ~A requires a non-empty description." name))
    (unless (non-empty-string-p tip)
      (application-command--definition-error
       name "Application command ~A requires a non-empty tip." name))
    (unless (member busy-behavior
                    *application-command-busy-behaviors*
                    :test #'eq)
      (application-command--definition-error
       name "Application command ~A has invalid busy behavior ~S." name busy-behavior))
    (unless (member terminal-behavior
                    *application-command-terminal-behaviors*
                    :test #'eq)
      (application-command--definition-error
       name "Application command ~A has invalid terminal behavior ~S." name terminal-behavior))
    (unless (or (null help-topic)
                (find help-topic *application-help-sections*
                      :key (lambda (section) (getf section :topic))))
      (application-command--definition-error
       name "Application command ~A has unknown help topic ~S." name help-topic))
    (unless (or (null help-order) (integerp help-order))
      (application-command--definition-error
       name "Application command ~A has invalid help order ~S." name help-order))
    t)

  (-> application-command--validate-defining-form (t t t) boolean)
  (defun application-command--validate-defining-form
      (definition-name metadata lambda-list)
    "Validate one literal DEFINE-APPLICATION-COMMAND header."
    (unless (and (symbolp definition-name)
                 definition-name
                 (not (keywordp definition-name))
                 (symbol-package definition-name))
      (application-command--definition-error
       definition-name
       "An application command definition name must be an interned non-keyword symbol."))
    (unless (and (proper-list-p metadata)
                 (evenp (length metadata)))
      (application-command--definition-error
       definition-name
       "Application command ~S metadata is not a literal property list."
       definition-name))
    (loop for key in metadata by (partial #'nthcdr 2)
          unless (member key *application-command-metadata-keys* :test #'eq)
            do (application-command--definition-error
                definition-name
                "Application command ~S has unknown metadata key ~S."
                definition-name
                key))
    (dolist (key *application-command-metadata-keys*)
      (when (> (application-command--metadata-key-count metadata key) 1)
        (application-command--definition-error
         definition-name "Application command ~S repeats metadata key ~S." definition-name key)))
    (dolist (key *application-command-required-metadata-keys*)
      (unless (= (application-command--metadata-key-count metadata key) 1)
        (application-command--definition-error
         definition-name
         "Application command ~S requires literal metadata key ~S."
         definition-name
         key)))
    (let ((static-options (getf metadata :static-options))
          (argument-count (application-command--metadata-key-count metadata :argument)))
      (unless (or (= argument-count 1) static-options)
        (application-command--definition-error
         definition-name
         "Application command ~S requires :ARGUMENT or :STATIC-OPTIONS."
         definition-name))
      (when (and (= argument-count 1) static-options)
        (application-command--definition-error
         definition-name
         "Application command ~S cannot specify both :ARGUMENT and :STATIC-OPTIONS."
         definition-name))
      (unless (and (proper-list-p lambda-list)
                   (plusp (length lambda-list))
                   (symbolp (first lambda-list))
                   (first lambda-list)
                   (not (keywordp (first lambda-list))))
        (application-command--definition-error
         definition-name
         "Application command ~S needs a required APPLICATION parameter."
         definition-name))
      (multiple-value-bind (minimum maximum)
          (application-command--lambda-list-arity (rest lambda-list))
        (when static-options
          (unless (application-command--static-options-p static-options)
            (application-command--definition-error
             definition-name
             "Application command ~S has invalid static options."
             definition-name))
          (unless (and (<= minimum 1)
                       maximum
                       (= maximum 1))
            (application-command--definition-error
             definition-name
             "Application command ~S static options require exactly one positional parameter."
             definition-name)))
        (application-command--validate-metadata
         (getf metadata :name)
         :aliases           (getf metadata :aliases)
         :argument          (if static-options
                                (application-command--static-option-hint static-options
                                                                         (zerop minimum))
                                (getf metadata :argument))
         :description       (getf metadata :description)
         :tip               (getf metadata :tip)
         :busy-behavior     (getf metadata :busy-behavior)
         :terminal-behavior (getf metadata :terminal-behavior)
         :static-options    static-options
         :help-topic        (getf metadata :help-topic)
         :help-order        (getf metadata :help-order))))))

(defclass application-command ()
  ((definition-name
    :initarg       :definition-name
    :reader        application-command-definition-name
    :type          symbol
    :documentation "The stable defining-form name used for replacement and replay.")
   (name
    :initarg       :name
    :reader        application-command-name
    :type          non-empty-string
    :documentation "The canonical lowercase native command name.")
   (aliases
    :initarg       :aliases
    :initform      nil
    :reader        application-command-aliases
    :type          list
    :documentation "Alternative native names sharing this command's behavior.")
   (argument
    :initarg       :argument
    :reader        application-command-argument
    :type          (option string)
    :documentation "The optional argument hint rendered by help and completion.")
   (description
    :initarg       :description
    :reader        application-command-description
    :type          non-empty-string
    :documentation "The concise help and completion description.")
   (tip
    :initarg       :tip
    :reader        application-command-tip
    :type          non-empty-string
    :documentation "The startup advice and help notes, with Markdown inline code for Lisp forms.")
   (help-topic
    :initarg       :help-topic
    :initform      nil
    :reader        application-command-help-topic
    :type          (option keyword)
    :documentation "The help section supplied by this registration, or NIL for Other commands.")
   (help-order
    :initarg       :help-order
    :initform      nil
    :reader        application-command-help-order
    :type          (option integer)
    :documentation "The editorial rank within its help section; unranked commands follow ranked ones.")
   (busy-behavior
    :initarg       :busy-behavior
    :reader        application-command-busy-behavior
     :type         (member :hold :inspect :execute :apply :apply-interactive)
    :documentation "The command policy while application work is active.")
   (terminal-behavior
    :initarg       :terminal-behavior
    :reader        application-command-terminal-behavior
    :type          (member :shared :exclusive :exclusive-without-arguments)
    :documentation "When command execution requires exclusive terminal input.")
   (lambda-list
    :initarg       :lambda-list
    :initform      nil
    :reader        application-command-lambda-list
    :type          list
    :documentation "The handler's parameter list after the application parameter.")
   (static-options
    :initarg       :static-options
    :initform      nil
    :reader        application-command-static-options
    :type          list
    :documentation "Finite native string, keyword, or boolean values offered for the command's sole argument.")
   (handler
    :initarg       :handler
    :reader        application-command-handler
    :type          function
    :documentation "The immutable behavior captured when this definition registered."))
  (:documentation
   "Immutable metadata and behavior for one canonical interactive command."))

(defclass application-command-invocation ()
  ((input
    :initarg       :input
    :reader        application-command-invocation-input
    :type          string
    :documentation "The complete submitted command input.")
   (syntax
    :initarg       :syntax
    :initform      ':lisp
    :reader        application-command-invocation-syntax
    :type          (member :lisp)
    :documentation "Native Lisp argument provenance for durable command invocations.")
   (name
    :initarg       :name
    :reader        application-command-invocation-name
    :type          string
    :documentation "The submitted command token normalized to lowercase.")
   (remainder
    :initarg       :remainder
    :reader        application-command-invocation-remainder
    :type          string
    :documentation "The trimmed text following the submitted command token.")
   (argument
    :initarg       :argument
    :reader        application-command-invocation-argument
    :type          (option string)
    :documentation "The first supplied argument in textual form, when available.")
   (arguments
    :initarg       :arguments
    :initform      nil
    :reader        application-command-invocation-arguments
    :type          list
    :documentation "The raw semantic argument values supplied by this invocation.")
   (supplied-argument-count
    :initarg       :supplied-argument-count
    :initform      0
    :reader        application-command-invocation-supplied-argument-count
    :type          (integer 0)
    :documentation "The number of semantic arguments explicitly supplied.")
   (command
    :initarg       :command
    :reader        application-command-invocation-command
    :type          (option application-command)
    :documentation "The canonical registered command resolved for this input."))
  (:documentation
   "One parsed command submission and its registry resolution snapshot."))

(defclass application-command-registration ()
  ((command
    :initarg       :command
    :reader        application-command-registration-command
    :type          application-command
    :documentation "The immutable command contributed by this registry layer.")
   (source
    :initarg       :source
    :reader        application-command-registration-source
    :type          keyword
    :documentation "The site, user, or runtime layer that contributed the command."))
  (:documentation "One ordered, possibly shadowed command registration layer."))

(defvar *application-command-registrations* nil
  "Ordered command registration layers, including shadowed definitions.")

(defvar *application-command-effective* nil
  "The effective canonical commands in deterministic presentation order.")

(defvar *application-command-index* (make-hash-table :test #'equal)
  "Canonical command names and aliases mapped to effective command objects.")

(defvar *application-command-lock*
  (make-lock "Autolith application commands")
  "The lock protecting all application command registry projections.")


;;;; -- Command Construction --

(-> application-command--validate (application-command) application-command)
(defun application-command--validate (command)
  "Validate COMMAND's complete immutable state and return it."
  (unless (and (symbolp (application-command-definition-name command))
               (application-command-definition-name command)
               (not
                (keywordp (application-command-definition-name command)))
               (symbol-package
                (application-command-definition-name command)))
    (error 'configuration-error
           :message
           "An application command definition name must be an interned non-keyword symbol."))
  (handler-case
      (application-command--validate-metadata
       (application-command-name command)
       :aliases           (application-command-aliases command)
       :argument          (application-command-argument command)
       :description       (application-command-description command)
       :tip               (application-command-tip command)
       :busy-behavior     (application-command-busy-behavior command)
       :terminal-behavior (application-command-terminal-behavior command)
       :static-options    (application-command-static-options command)
       :help-topic        (application-command-help-topic command)
       :help-order        (application-command-help-order command))
    (error (condition)
      (error 'configuration-error
             :message (princ-to-string condition))))
  (unless (functionp (application-command-handler command))
    (error 'configuration-error
           :message (format nil "Application command ~A has no callable handler."
                            (application-command-name command))))
  (handler-case
      (multiple-value-bind (minimum maximum)
          (application-command--lambda-list-arity (application-command-lambda-list command))
        (when (application-command-static-options command)
          (unless (and (<= minimum 1)
                       maximum
                       (= maximum 1))
            (application-command--definition-error
             (application-command-name command)
             "Static command options require exactly one positional parameter."))))
    (error (condition)
      (error 'configuration-error :message (princ-to-string condition))))
  command)

(-> application-command-create
    (&key (:definition-name symbol) (:name string) (:aliases list)
          (:argument (option string)) (:description string) (:tip string)
          (:busy-behavior keyword) (:terminal-behavior keyword)
          (:lambda-list list) (:static-options list)
          (:handler function)
          (:help-topic t) (:help-order t))
    application-command)
(defun application-command-create
    (&key definition-name name aliases argument description tip busy-behavior
            terminal-behavior lambda-list static-options handler
            help-topic help-order)
  "Create and validate one immutable interactive command."
  (unless (and (symbolp definition-name)
               definition-name
               (not (keywordp definition-name))
               (symbol-package definition-name))
    (error 'configuration-error
           :message
           "An application command definition name must be an interned non-keyword symbol."))
  (handler-case
      (progn
        (when static-options
          (multiple-value-bind (minimum maximum)
              (application-command--lambda-list-arity lambda-list)
            (declare (ignore maximum))
            (setf argument
                  (application-command--static-option-hint
                   static-options
                   (zerop minimum)))))
        (application-command--validate-metadata name
                                                :aliases           aliases
                                                :argument          argument
                                                :description       description
                                                :tip               tip
                                                :busy-behavior     busy-behavior
                                                :terminal-behavior terminal-behavior
                                                :static-options    static-options
                                                :help-topic        help-topic
                                                :help-order        help-order))
    (error (condition)
      (error 'configuration-error :message (princ-to-string condition))))
  (unless (functionp handler)
    (error 'configuration-error
           :message (format nil "Application command ~A has no callable handler."
                            name)))
  (application-command--validate
   (make-instance
    'application-command
    :definition-name   definition-name
    :name              (copy-seq name)
    :aliases           (mapcar #'copy-seq aliases)
    :argument          (and argument (copy-seq argument))
    :description       (copy-seq description)
    :tip               (copy-seq tip)
    :busy-behavior     busy-behavior
    :terminal-behavior terminal-behavior
    :help-topic        help-topic
    :help-order        help-order
    :lambda-list       (copy-tree lambda-list)
    :static-options    (mapcar (lambda (option)
                                 (if (stringp option) (copy-seq option) option))
                               static-options)
    :handler           handler)))

(defgeneric application-command-validate-registration (command)
  (:documentation
   "Validate COMMAND against loaded operation surfaces and return COMMAND."))

(defmethod application-command-validate-registration ((command t))
  "Permit COMMAND before operation-specific registration checks are loaded."
  command)


;;;; -- Layered Registry --

(-> application-command--current-registration-source () keyword)
(defun application-command--current-registration-source ()
  "Return the registration source appropriate to the current load context."
  *extension-registration-source*)

(-> application-command--effective-projections
    (list)
    (values list hash-table))
(defun application-command--effective-projections (registrations)
  "Return validated effective command order and identifier index."
  (dolist (registration registrations)
    (unless (typep registration 'application-command-registration)
      (error 'configuration-error
             :message "The application command registry contains an invalid layer."))
    (application-command--validate
     (application-command-registration-command registration)))
  (let ((effective
          (layered-registry-effective
           registrations
           (lambda (registration)
             (application-command-name
              (application-command-registration-command registration)))
           (constantly 0)))
        (index (make-hash-table :test #'equal)))
    (dolist (registration effective)
      (let ((command (application-command-registration-command registration)))
        (dolist (identifier
                 (cons (application-command-name command)
                       (application-command-aliases command)))
          (let ((existing (gethash identifier index)))
            (when (and existing (not (eq existing command)))
              (error 'configuration-error
                     :message
                     (format nil
                             "Application command identifier ~A belongs to both ~A and ~A."
                             identifier
                             (application-command-name existing)
                             (application-command-name command)))))
          (setf (gethash identifier index) command))))
    (values (mapcar #'application-command-registration-command effective)
            index)))

(-> application-command--publish-registrations (list) null)
(defun application-command--publish-registrations (registrations)
  "Validate and publish REGISTRATIONS while the registry lock is held."
  (multiple-value-bind (effective index)
      (application-command--effective-projections registrations)
    (setf *application-command-registrations* registrations
          *application-command-effective* effective
          *application-command-index* index))
  (extension-registry--advance-generation)
  nil)

(-> register-application-command
    (application-command &key (:source keyword))
    application-command)
(defun register-application-command
    (command &key (source (application-command--current-registration-source)))
  "Register immutable COMMAND in SOURCE and return COMMAND.

The same source and definition name replace their prior layer in place. A
different definition may shadow the same canonical command without destroying
the earlier layer. Identifier collisions among effective commands are rejected
without changing the registry."
  (application-command--validate command)
  (application-command-validate-registration command)
  (unless (keywordp source)
    (error 'configuration-error
           :message "An application command registration source must be a keyword."))
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (let* ((replacement
               (make-instance 'application-command-registration
                              :command command
                              :source  source))
             (candidate
               (layered-registry-replace
                *application-command-registrations* replacement
                :key-function
                (lambda (registration)
                  (application-command-definition-name
                   (application-command-registration-command registration)))
                :source-function #'application-command-registration-source
                :key-test        #'eq
                :source          source)))
        (application-command--publish-registrations candidate))))
  command)

(-> unregister-application-command
    (symbol &key (:source keyword))
    boolean)
(defun unregister-application-command
    (definition-name
     &key (source (application-command--current-registration-source)))
  "Remove DEFINITION-NAME's registration from SOURCE and report a change."
  (unless (and (symbolp definition-name)
               definition-name
               (not (keywordp definition-name))
               (symbol-package definition-name))
    (error 'configuration-error
           :message
           "An application command definition name must be an interned non-keyword symbol."))
  (unless (keywordp source)
    (error 'configuration-error
           :message "An application command registration source must be a keyword."))
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (let ((candidate
              (layered-registry-remove
               *application-command-registrations* definition-name
               :key-function
               (lambda (registration)
                 (application-command-definition-name
                  (application-command-registration-command registration)))
               :source-function #'application-command-registration-source
               :key-test        #'eq
               :source          source)))
        (if (= (length candidate)
               (length *application-command-registrations*))
            nil
            (progn
              (application-command--publish-registrations candidate)
              t))))))

(-> application-command-list () list)
(defun application-command-list ()
  "Return an ordered snapshot of effective canonical commands."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (copy-list *application-command-effective*))))

(-> application-command-find (string) (option application-command))
(defun application-command-find (identifier)
  "Return the effective command named by case-insensitive IDENTIFIER."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (gethash (string-downcase identifier) *application-command-index*))))

(-> application-command--registrations () list)
(defun application-command--registrations ()
  "Return detached descriptions of every ordered command registration layer."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (loop for registration in *application-command-registrations*
            for position from 0
            for command =
              (application-command-registration-command registration)
            collect (list :position position
                          :source
                          (application-command-registration-source registration)
                          :definition-name
                          (application-command-definition-name command)
                          :command  command)))))

(-> application-command--registry-snapshot () list)
(defun application-command--registry-snapshot ()
  "Return an exact ordered snapshot of command registration layers."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (copy-list *application-command-registrations*))))

(-> application-command--registry-restore (list) null)
(defun application-command--registry-restore (snapshot)
  "Atomically replace command registrations with exact ordered SNAPSHOT."
  (unless (proper-list-p snapshot)
    (error 'configuration-error
           :message "An application command registry snapshot must be a proper list."))
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (application-command--publish-registrations (copy-list snapshot))))
  nil)

(-> application-command--remove-registration-source (keyword) null)
(defun application-command--remove-registration-source (source)
  "Remove every command registration contributed by SOURCE."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (application-command--publish-registrations
       (remove source
               *application-command-registrations*
               :test #'eq
               :key  #'application-command-registration-source))))
  nil)

(-> application-command--registration-snapshot
    (symbol keyword)
    (option list))
(defun application-command--registration-snapshot (definition-name source)
  "Return DEFINITION-NAME's SOURCE registration and exact position."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (loop for registration in *application-command-registrations*
            for position from 0
            when
              (and
               (eq source
                   (application-command-registration-source registration))
               (eq definition-name
                   (application-command-definition-name
                    (application-command-registration-command registration))))
              return (list :position position :registration registration)))))

(-> application-command--registration-restore
    (symbol keyword (option list))
    null)
(defun application-command--registration-restore
    (definition-name source snapshot)
  "Restore one registration to exact SNAPSHOT position, or remove it."
  (with-extension-registry-transaction
    (with-lock-held (*application-command-lock*)
      (let* ((remaining
               (remove-if
                (lambda (registration)
                  (and
                   (eq source
                       (application-command-registration-source registration))
                   (eq definition-name
                       (application-command-definition-name
                        (application-command-registration-command
                         registration)))))
                *application-command-registrations*))
             (candidate
               (if snapshot
                   (let ((position     (getf snapshot :position))
                         (registration (getf snapshot :registration)))
                     (unless (and (typep position '(integer 0))
                                  (typep registration
                                         'application-command-registration))
                       (error 'configuration-error
                              :message
                              "An application command registration snapshot is invalid."))
                     (let ((bounded-position
                             (min position (length remaining))))
                       (append (subseq remaining 0 bounded-position)
                               (list registration)
                               (nthcdr bounded-position remaining))))
                   remaining)))
        (application-command--publish-registrations candidate))))
  nil)


;;;; -- Invocation and Dispatch --

(defvar *application-command-presentation-invocation* nil
  "The command invocation whose next presentation receives a visible heading.")

(defvar *application-command-presentation-pending-p* nil
  "Whether the current command invocation still needs its presentation heading.")

(defvar *application-command-presentation-text* nil
  "The reverse-ordered finalized text captured from command presentations.")

(defvar *application-command-presentation-capture-p* nil
  "Whether finalized command presentations should be collected for ACP output.")

(defvar *application-command-interactive-p* nil
  "Whether the current local command call may acquire omitted arguments interactively.")

(-> application-command--display-name (application-command) string)
(defun application-command--display-name (command)
  "Return COMMAND's canonical callable spelling for human-facing diagnostics."
  (format nil "(~A)" (application-command-name command)))

(-> application-command--validate-invocation-arity
    (application-command list)
    list)
(defun application-command--validate-invocation-arity (command arguments)
  "Validate and return semantic ARGUMENTS before invoking COMMAND."
  (multiple-value-bind (minimum maximum)
      (application-command--lambda-list-arity
       (application-command-lambda-list command))
    (declare (ignore minimum))
    (when (and maximum (> (length arguments) maximum))
      (error 'configuration-error
             :message
             (format nil
                     "Command ~A accepts at most ~D argument~:P; received ~D."
                     (application-command--display-name command)
                     maximum
                     (length arguments)))))
  arguments)

(-> application-command--call-with-presentation
    (application-command-invocation function)
    t)
(defun application-command--call-with-presentation (invocation function)
  "Call FUNCTION with INVOCATION available to label its first presentation."
  (let ((*application-command-presentation-invocation* invocation)
        (*application-command-presentation-pending-p*  t))
    (funcall function)))

(defgeneric application-command-execute (command application invocation)
  (:documentation
   "Execute COMMAND for APPLICATION and INVOCATION; return :CONTINUE and the command's value."))

(defmethod application-command-execute
    ((command application-command)
     application
     (invocation application-command-invocation))
  "Invoke COMMAND's captured handler and validate its completion status."
  (labels ((invoke-handler (arguments)
             "Invoke COMMAND's handler with replaceable ARGUMENTS."
             (if *application-command-interactive-p*
                 (restart-case
                     (apply (application-command-handler command)
                            application arguments)
                   (supply-arguments (&rest replacement-arguments)
                     :report
                     (lambda (stream)
                       (format stream "Supply replacement arguments for ~A."
                               (application-command--display-name command)))
                     (invoke-handler replacement-arguments)))
                 (apply (application-command-handler command)
                        application arguments)))

           (invoke ()
             "Invoke COMMAND's handler and validate its result."
             (let ((arguments (application-command-invocation-arguments invocation)))
               (application-command--validate-invocation-arity command arguments)
               (multiple-value-bind (action value) (invoke-handler arguments)
                 (unless (eq action ':continue)
                   (error 'configuration-error
                          :message
                          (format nil
                                  "Application command ~A returned invalid action ~S."
                                  (application-command--display-name command)
                                  action)))
                 (values action value)))))
    (if (eq *application-command-presentation-invocation* invocation)
        (invoke)
        (application-command--call-with-presentation invocation #'invoke))))

(-> application-command-invocation-argument-free-p
    (application-command application-command-invocation)
    boolean)
(defun application-command-invocation-argument-free-p (command invocation)
  "Return whether INVOCATION supplied no native command arguments."
  (declare (ignore command))
  (zerop (application-command-invocation-supplied-argument-count invocation)))

(defgeneric application-command-busy-action (command invocation)
  (:documentation
   "Return :HOLD, :EXECUTE, :APPLY, or :APPLY-INTERACTIVE.

:EXECUTE runs immediately. :APPLY and :APPLY-INTERACTIVE run at the next safe
provider boundary before steering, with the latter allowing a terminal picker.
:HOLD waits for idle."))

(defmethod application-command-busy-action
    ((command application-command)
     (invocation application-command-invocation))
  "Resolve COMMAND's declared busy policy for INVOCATION.

Argument-free :INSPECT queries execute immediately. Argument-free :APPLY
queries do likewise unless they require exclusive terminal ownership.
:APPLY-INTERACTIVE settings may use a picker at the boundary when called with
no arguments; explicit values still use the noninteractive :APPLY lane."
  (ecase (application-command-busy-behavior command)
    (:hold
     ':hold)
    (:inspect
     (if (application-command-invocation-argument-free-p command invocation)
         ':execute
         ':hold))
    (:apply-interactive
     (if (and (application-command-invocation-argument-free-p command invocation)
              (application-command-terminal-owner-p command invocation))
         ':apply-interactive
         ':apply))
    (:apply
     (cond
       ((not (application-command-invocation-argument-free-p command invocation))
        ':apply)
       ((application-command-terminal-owner-p command invocation)
        ':hold)
       (t
        ':execute)))
    (:execute
     ':execute)))

(defgeneric application-command-terminal-owner-p (command invocation)
  (:documentation
   "Return whether COMMAND requires exclusive terminal input for INVOCATION."))

(defmethod application-command-terminal-owner-p
    ((command application-command)
     (invocation application-command-invocation))
  "Resolve COMMAND's terminal policy for INVOCATION."
  (case (application-command-terminal-behavior command)
    (:shared
     nil)
    (:exclusive
     t)
    (:exclusive-without-arguments
     (application-command-invocation-argument-free-p command invocation))
    (otherwise
     nil)))


;;;; -- Defining Form --

(defmacro define-application-command
    (definition-name metadata lambda-list &body body)
  "Define and register one complete, live-redefinable application command.

METADATA must contain literal :NAME, :DESCRIPTION, :TIP, :BUSY-BEHAVIOR, and
:TERMINAL-BEHAVIOR values. :ALIASES defaults to NIL. Commands state either an
:ARGUMENT hint or finite :STATIC-OPTIONS. The handler's parameters after
APPLICATION are its Lisp argument contract. The handler is captured at
registration. :HELP-TOPIC selects a supported section, defaulting to Other
commands. :HELP-ORDER is an optional integer rank, with ties. Replacements
supply their own help metadata. Every handler returns :CONTINUE and may return
a second value, which a Lisp call of the command yields."
  (application-command--validate-defining-form
   definition-name metadata lambda-list)
  (let ((static-options   (getf metadata :static-options))
        (call-lambda-list (rest lambda-list)))
    `(progn
       (defun ,definition-name ,lambda-list
         ,@body)
       (eval-when (:load-toplevel :execute)
         (register-application-command
          (application-command-create
           :definition-name   ',definition-name
           :name              ,(getf metadata :name)
           :aliases           ',(getf metadata :aliases)
           :argument          ,(getf metadata :argument)
           :description       ,(getf metadata :description)
           :tip               ,(getf metadata :tip)
           :busy-behavior     ',(getf metadata :busy-behavior)
           :terminal-behavior ',(getf metadata :terminal-behavior)
           :help-topic        ',(getf metadata :help-topic)
           :help-order        ,(getf metadata :help-order)
           :lambda-list       ',call-lambda-list
           :static-options    ',static-options
           :handler           #',definition-name)))
       ',definition-name)))


;;;; -- Conditions --

(define-condition application-command-definition-error (configuration-error)
  ((command
    :initarg       :command
    :initform      nil
    :reader        application-command-definition-error-command
    :type          t
    :documentation "The command name or definition symbol at fault, or NIL for a lambda-list failure."))
  (:documentation "An application command definition has invalid metadata or an unsupported lambda list."))
