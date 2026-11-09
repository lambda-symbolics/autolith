(in-package #:autolith)

;;;; -- Native MCP Configuration --

(defparameter *mcp-configuration-version* 1
  "The only native MCP configuration version accepted by Autolith.")

(defparameter *mcp-configuration-maximum-bytes* (* 256 1024)
  "The maximum byte length of one native MCP configuration file.")

(defparameter *mcp-configuration-maximum-servers* 64
  "The maximum number of effective MCP servers.")

(defparameter *mcp-configuration-maximum-depth* 64
  "The maximum nested list depth in native MCP configuration.")

(defparameter *mcp-configuration-maximum-nodes* 32768
  "The maximum number of values in native MCP configuration.")

(defparameter *mcp-server-name-maximum-characters* 64
  "The maximum character length of an MCP server name.")

(defparameter *mcp-child-tool-name-maximum-characters* 256
  "The maximum character length of one raw MCP child tool name.")

(defparameter *mcp-maximum-child-tools* 128
  "The maximum number of tools one MCP server may grant to child agents.")

(defparameter *mcp-maximum-trusted-read-only-tools* 128
  "The maximum exact tool names trusted for read-only MCP annotations.")

(defparameter *mcp-default-startup-timeout-seconds* 15
  "The default timeout for MCP initialization and discovery, in seconds.")

(defparameter *mcp-default-tool-timeout-seconds* 60
  "The default timeout for an MCP tool call, in seconds.")

(defparameter *mcp-maximum-timeout-seconds* 3600
  "The maximum configurable MCP transport or operation timeout, in seconds.")

(defparameter *mcp-approval-policies*
  '(:read-only :prompt :allow :deny)
  "The MCP call policies accepted by native configuration.")

(defparameter *mcp-configuration-native-keywords*
  '(:version :servers :directories
    :name :transport :required-p :startup-timeout-seconds
    :tool-timeout-seconds :approval :trusted-read-only-tools :child-tools
    :type :stdio :command :arguments :directory :workspace :environment
    :http :url :headers :connect-timeout-seconds
    :read-only :prompt :allow :deny)
  "Every keyword token accepted by the native MCP data grammar.")

(defparameter *mcp-registration-source-precedence*
  '((:tracked . 0)
    (:site-config . 1)
    (:site . 2)
    (:config . 3)
    (:directory . 4)
    (:user . 5)
    (:runtime . 6))
  "The explicit low-to-high precedence of MCP registration sources.")

(define-condition mcp-configuration-error (configuration-error)
  ((pathname
    :initarg       :pathname
    :initform      nil
    :reader        mcp-configuration-error-pathname
    :type          (option pathname)
    :documentation "The native MCP configuration file involved, when any.")
   (server-name
    :initarg       :server-name
    :initform      nil
    :reader        mcp-configuration-error-server-name
    :type          (option string)
    :documentation "The server definition involved, when known.")
   (field
    :initarg       :field
    :initform      nil
    :reader        mcp-configuration-error-field
    :type          (option keyword)
    :documentation "The invalid native field, when known.")
   (cause
    :initarg       :cause
    :initform      nil
    :reader        mcp-configuration-error-cause
    :type          t
    :documentation "The underlying reader or validation failure, when any."))
  (:documentation "A native Autolith MCP configuration is malformed."))

(defclass mcp-server-configuration ()
  ((name
    :initarg       :name
    :reader        mcp-server-configuration-name
    :type          non-empty-string
    :documentation "The user-facing, case-sensitive server identifier.")
   (transport
    :initarg       :transport
    :reader        mcp-server-configuration-transport
    :type          mcp-transport-configuration
    :documentation "The transport used to reach this server.")
   (required-p
    :initarg       :required-p
    :initform      nil
    :reader        mcp-server-configuration-required-p
    :type          boolean
    :documentation "Whether discovery failure prevents Autolith startup.")
   (startup-timeout-seconds
    :initarg       :startup-timeout-seconds
    :initform      *mcp-default-startup-timeout-seconds*
    :reader        mcp-server-configuration-startup-timeout-seconds
    :type          real
    :documentation "The server initialization and discovery deadline.")
   (tool-timeout-seconds
    :initarg       :tool-timeout-seconds
    :initform      *mcp-default-tool-timeout-seconds*
    :reader        mcp-server-configuration-tool-timeout-seconds
    :type          real
    :documentation "The default deadline for one server tool call.")
   (approval-policy
    :initarg       :approval-policy
    :initform      ':prompt
    :reader        mcp-server-configuration-approval-policy
    :type          keyword
    :documentation "The policy deciding which MCP tool calls need approval.")
   (trusted-read-only-tools
    :initarg  :trusted-read-only-tools
    :initform nil
    :reader   mcp-server-configuration-trusted-read-only-tools
    :type     list
    :documentation
    "Exact raw tools whose read-only annotations the user explicitly trusts.")
   (child-tools
    :initarg       :child-tools
    :initform      nil
    :reader        mcp-server-configuration-child-tools
    :type          list
    :documentation "Exact raw MCP tool names explicitly granted to child agents."))
  (:documentation "One complete native Autolith MCP server definition."))

(defclass mcp-server-registration ()
  ((configuration
    :initarg       :configuration
    :reader        mcp-server-registration-configuration
    :type          mcp-server-configuration
    :documentation "The immutable registered server configuration.")
   (source
    :initarg :source
    :reader  mcp-server-registration-source
    :type    keyword
    :documentation
    "The tracked, site-config, site, config, directory, user, or runtime registration layer."))
  (:documentation "One source-attributed layer in the MCP server registry."))


;;;; -- Native Form Validation --

(-> configuration-mcp-path (configuration) pathname)
(defun configuration-mcp-path (configuration)
  "Return CONFIGURATION's native versioned MCP file."
  (merge-pathnames "mcp.sexp" (config :config-root configuration)))

(-> mcp-configuration--error
    (string &key (:pathname (option pathname))
                 (:server-name (option string))
                 (:field (option keyword))
                 (:cause t))
    nil)
(defun mcp-configuration--error
    (message &key pathname server-name field cause)
  "Signal a structured native MCP configuration failure."
  (error 'mcp-configuration-error
         :message     message
         :pathname    pathname
         :server-name server-name
         :field       field
         :cause       cause))

(-> mcp-configuration--bounded-string-p
    (t integer &key (:empty-p boolean))
    boolean)
(defun mcp-configuration--bounded-string-p
    (value maximum-characters &key empty-p)
  "Return true when VALUE is a bounded, single-line string.

An empty string is accepted only when EMPTY-P is true. NUL and terminal
control characters are rejected even when the native reader accepted them."
  (and (stringp value)
       (<= (length value) maximum-characters)
       (or empty-p (plusp (length value)))
       (loop for character across value
             always
             (and (not (char= character #\Null))
                  (or (graphic-char-p character)
                      (char= character #\Space))))))

(-> mcp-configuration--native-value-p (t) boolean)
(defun mcp-configuration--native-value-p (value)
  "Return true when VALUE is one atom native MCP configuration may contain."
  (not (null (or (null value)
                 (eq value t)
                 (keywordp value)
                 (stringp value)
                 (realp value)))))

(-> mcp-configuration--source-grammar () source-grammar)
(defun mcp-configuration--source-grammar ()
  "Return the bounded native data grammar one MCP configuration may use.

The grammar is rebuilt for every read so that a live change to the keyword or
bound policy takes effect without reloading this file."
  (make-source-grammar
   :label                      "Native MCP configuration"
   :keywords                   *mcp-configuration-native-keywords*
   :maximum-depth              *mcp-configuration-maximum-depth*
   :maximum-nodes              *mcp-configuration-maximum-nodes*
   :improper-lists-permitted-p t
   :allowed-atom-predicate     #'mcp-configuration--native-value-p))

(-> mcp-configuration--validate-readable-tree
    (t &key (:pathname (option pathname)))
    t)
(defun mcp-configuration--validate-readable-tree (value &key pathname)
  "Reject shared, circular, or non-native objects in readable VALUE."
  (handler-case
      (validate-tree value (mcp-configuration--source-grammar))
    (sexp-config-error (condition)
      (mcp-configuration--error (sexp-config-error-message condition)
                                :pathname pathname))))

(-> mcp-configuration--validate-plist
    (t list &key (:pathname (option pathname))
                 (:server-name (option string)))
    list)
(defun mcp-configuration--validate-plist
    (value allowed-keys &key pathname server-name)
  "Return VALUE after validating a proper keyword plist against ALLOWED-KEYS."
  (multiple-value-bind (problem key)
      (plist-schema-problem value :allowed-keys allowed-keys)
    (when problem
      (mcp-configuration--error
       (case problem
         (:improper
          "An MCP native object must be a proper property list.")
         (:odd
          "An MCP native object has a property without a value.")
         (:non-keyword
          (format nil "MCP configuration key ~S is not a keyword." key))
         (:unknown
          (format nil "Unknown MCP configuration key ~S." key))
         (:duplicate
          (format nil "Duplicate MCP configuration key ~S." key)))
       :pathname    pathname
       :server-name server-name
       :field       (and (member problem '(:unknown :duplicate)) key))))
  value)

(-> mcp-configuration--property
    (list keyword &key (:required-p boolean)
                  (:pathname (option pathname))
                  (:server-name (option string)))
    t)
(defun mcp-configuration--property
    (properties key &key required-p pathname server-name)
  "Return KEY from PROPERTIES and reject an absent required value."
  (loop for (candidate value) on properties by (partial #'nthcdr 2)
        when (eq candidate key)
          do (return-from mcp-configuration--property value))
  (when required-p
    (mcp-configuration--error
     (format nil "MCP configuration is missing required key ~S." key)
     :pathname    pathname
     :server-name server-name
     :field       key))
  nil)

(-> mcp-configuration--property-present-p (list keyword) boolean)
(defun mcp-configuration--property-present-p (properties key)
  "Return true when PROPERTIES explicitly contains KEY."
  (loop for tail on properties by (partial #'nthcdr 2)
        thereis (eq (first tail) key)))

(-> mcp-configuration--positive-timeout
    (t keyword &key (:pathname (option pathname))
                    (:server-name (option string)))
    real)
(defun mcp-configuration--positive-timeout
    (value field &key pathname server-name)
  "Return a positive bounded real timeout VALUE or reject FIELD."
  (unless (and (realp value)
               (plusp value)
               (<= value *mcp-maximum-timeout-seconds*))
    (mcp-configuration--error
     (format nil
             "MCP timeout ~S must be positive and no greater than ~D seconds."
             field
             *mcp-maximum-timeout-seconds*)
     :pathname    pathname
     :server-name server-name
     :field       field))
  value)

(-> mcp-configuration--transport
    (t &key (:pathname (option pathname))
            (:server-name (option string)))
    mcp-transport-configuration)
(defun mcp-configuration--transport (form &key pathname server-name)
  "Read a shared declarative transport and attach native error context."
  (handler-case
      (mcparen:mcp-read-transport-configuration
       form :maximum-timeout-seconds *mcp-maximum-timeout-seconds*)
    (mcparen:mcp-configuration-error (condition)
      (mcp-configuration--error
       (mcparen:mcp-error-message condition)
       :pathname pathname :server-name server-name
       :field (mcparen:mcp-configuration-error-field condition)))))

(-> mcp-server-configuration-create
    (&key (:name t)
          (:transport t)
          (:required-p t)
          (:startup-timeout-seconds t)
          (:tool-timeout-seconds t)
          (:approval t)
          (:trusted-read-only-tools t)
          (:child-tools t)
          (:pathname (option pathname)))
    mcp-server-configuration)
(defun mcp-server-configuration-create
    (&key name transport
      (required-p nil)
      (startup-timeout-seconds *mcp-default-startup-timeout-seconds*)
      (tool-timeout-seconds *mcp-default-tool-timeout-seconds*)
      (approval :prompt)
      (trusted-read-only-tools nil)
      (child-tools nil)
      pathname)
  "Create one validated MCP server configuration from native Lisp values."
  (unless
      (mcp-configuration--bounded-string-p
       name *mcp-server-name-maximum-characters*)
    (mcp-configuration--error
     "An MCP server name must be a bounded non-empty string."
     :pathname pathname
     :field    ':name))
  (unless (or (null required-p) (eq required-p t))
    (mcp-configuration--error
     "MCP :REQUIRED-P must be exactly T or NIL."
     :pathname    pathname
     :server-name name
     :field       ':required-p))
  (unless (member approval *mcp-approval-policies*)
    (mcp-configuration--error
     "Unsupported MCP approval policy."
     :pathname    pathname
     :server-name name
     :field       ':approval))
  (unless
      (and
       (proper-list-p trusted-read-only-tools)
       (<= (length trusted-read-only-tools)
           *mcp-maximum-trusted-read-only-tools*)
       (every
        (lambda (tool-name)
          (mcp-configuration--bounded-string-p
           tool-name
           *mcp-child-tool-name-maximum-characters*))
        trusted-read-only-tools)
       (= (length trusted-read-only-tools)
          (length
           (remove-duplicates trusted-read-only-tools :test #'string=))))
    (mcp-configuration--error
     "MCP :TRUSTED-READ-ONLY-TOOLS must be a bounded proper list of unique bounded raw tool names."
     :pathname    pathname
     :server-name name
     :field       ':trusted-read-only-tools))
  (when (and trusted-read-only-tools
             (not (eq approval :read-only)))
    (mcp-configuration--error
     "MCP :TRUSTED-READ-ONLY-TOOLS requires :APPROVAL :READ-ONLY."
     :pathname    pathname
     :server-name name
     :field       ':trusted-read-only-tools))
  (unless (and (proper-list-p child-tools)
               (<= (length child-tools) *mcp-maximum-child-tools*)
               (every
                (lambda (tool-name)
                  (mcp-configuration--bounded-string-p
                   tool-name
                   *mcp-child-tool-name-maximum-characters*))
                child-tools)
               (= (length child-tools)
                  (length (remove-duplicates child-tools :test #'string=))))
    (mcp-configuration--error
     "MCP :CHILD-TOOLS must be a bounded proper list of unique bounded raw tool names."
     :pathname    pathname
     :server-name name
     :field       ':child-tools))
  (make-instance
   'mcp-server-configuration
   :name            (copy-seq name)
   :transport
   (mcp-configuration--transport
    transport
    :pathname    pathname
    :server-name name)
   :required-p      (and required-p t)
   :startup-timeout-seconds
   (mcp-configuration--positive-timeout
    startup-timeout-seconds
    :startup-timeout-seconds
    :pathname pathname
    :server-name name)
   :tool-timeout-seconds
   (mcp-configuration--positive-timeout
    tool-timeout-seconds
    :tool-timeout-seconds
    :pathname pathname
    :server-name name)
   :approval-policy approval
   :trusted-read-only-tools
   (mapcar #'copy-seq trusted-read-only-tools)
   :child-tools     (mapcar #'copy-seq child-tools)))

(-> mcp-configuration--server
    (t &key (:pathname (option pathname)))
    mcp-server-configuration)
(defun mcp-configuration--server (form &key pathname)
  "Parse one strict native MCP server FORM."
  (mcp-configuration--validate-plist
   form
   '(:name :transport :required-p :startup-timeout-seconds
     :tool-timeout-seconds :approval :trusted-read-only-tools :child-tools)
   :pathname pathname)
  (let ((name
          (mcp-configuration--property
           form :name :required-p t :pathname pathname)))
    (mcp-server-configuration-create
     :name     name
     :transport
     (mcp-configuration--property
      form :transport
      :required-p t
      :pathname    pathname
      :server-name (and (stringp name) name))
     :required-p
     (or (mcp-configuration--property form :required-p) nil)
     :startup-timeout-seconds
     (if (mcp-configuration--property-present-p
          form :startup-timeout-seconds)
         (mcp-configuration--property form :startup-timeout-seconds)
         *mcp-default-startup-timeout-seconds*)
     :tool-timeout-seconds
     (if (mcp-configuration--property-present-p
          form :tool-timeout-seconds)
         (mcp-configuration--property form :tool-timeout-seconds)
         *mcp-default-tool-timeout-seconds*)
     :approval
     (if (mcp-configuration--property-present-p form :approval)
         (mcp-configuration--property form :approval)
         :prompt)
     :trusted-read-only-tools
     (or
      (mcp-configuration--property form :trusted-read-only-tools)
      nil)
     :child-tools
     (or (mcp-configuration--property form :child-tools) nil)
     :pathname pathname)))

(-> mcp-configuration--read-source (pathname) string)
(defun mcp-configuration--read-source (pathname)
  "Read regular PATHNAME once as bounded UTF-8 without blocking or racing."
  (handler-case
      (read-file-text pathname :maximum-octets *mcp-configuration-maximum-bytes*
                               :follow-links-p t)
    (not-regular-file ()
      (mcp-configuration--error
       "The native MCP configuration must be a regular file."
       :pathname pathname))
    (file-too-large ()
      (mcp-configuration--error
       "The native MCP configuration exceeds its byte bound."
       :pathname pathname))
    (serious-condition (cause)
      (mcp-configuration--error
       (format nil "Could not read native MCP configuration at ~A: ~A"
               pathname cause)
       :pathname pathname
       :cause    cause))))

(-> mcp-configuration--source-present-p
    (pathname &key (:description string))
    boolean)
(defun mcp-configuration--source-present-p
    (pathname &key (description "native MCP configuration"))
  "Return true when PATHNAME resolves to a regular file named by DESCRIPTION."
  (handler-case
      (let ((status (platform-path-status *platform* pathname
                                          :follow-links-p t)))
        (cond
          ((null status)
           (when (platform-path-status *platform* pathname)
             (mcp-configuration--error
              (format nil "The ~A link has no regular target." description)
              :pathname pathname))
           nil)
          ((eq (platform-file-status-kind status) ':file)
           t)
          (t
           (mcp-configuration--error
            (format nil "The ~A must be a regular file." description)
            :pathname pathname))))
    (platform-error (condition)
      (mcp-configuration--error
       (format nil "Could not inspect ~A at ~A: ~A"
               description pathname condition)
       :pathname pathname
       :cause    condition))))

(-> mcp-configuration--read-form (pathname) t)
(defun mcp-configuration--read-form (pathname)
  "Read exactly one bounded native MCP form from PATHNAME."
  (let ((source (mcp-configuration--read-source pathname)))
    (handler-case
        (read-source source (mcp-configuration--source-grammar))
      (sexp-config-error (condition)
        (mcp-configuration--error (sexp-config-error-message condition)
                                  :pathname pathname)))))

(-> mcp-configuration-read-path (pathname) list)
(defun mcp-configuration-read-path (pathname)
  "Read and validate native MCP server definitions from PATHNAME."
  (unless (mcp-configuration--source-present-p pathname)
    (return-from mcp-configuration-read-path nil))
  (let ((form (mcp-configuration--read-form pathname)))
    (mcp-configuration--validate-plist
     form '(:version :servers) :pathname pathname)
    (unless (eql
             (mcp-configuration--property
              form :version :required-p t :pathname pathname)
             *mcp-configuration-version*)
      (mcp-configuration--error
       (format nil "MCP configuration must use version ~D."
               *mcp-configuration-version*)
       :pathname pathname
       :field    ':version))
    (let ((servers
            (mcp-configuration--property
             form :servers :required-p t :pathname pathname)))
      (unless (proper-list-p servers)
        (mcp-configuration--error
         "MCP :SERVERS must be a proper list."
         :pathname pathname
         :field    ':servers))
      (when (> (length servers) *mcp-configuration-maximum-servers*)
        (mcp-configuration--error
         (format nil "MCP :SERVERS exceeds the limit of ~D."
                 *mcp-configuration-maximum-servers*)
         :pathname pathname
         :field    ':servers))
      (let ((definitions
              (mapcar
               (lambda (server)
                 (mcp-configuration--server server :pathname pathname))
               servers))
            (seen (make-hash-table :test #'equal)))
        (dolist (definition definitions)
          (let ((name (mcp-server-configuration-name definition)))
            (when (gethash name seen)
              (mcp-configuration--error
               (format nil "Duplicate MCP server name ~S." name)
               :pathname    pathname
               :server-name name
               :field       ':name))
            (setf (gethash name seen) t)))
        definitions))))

(-> mcp-configuration-read (configuration) list)
(defun mcp-configuration-read (configuration)
  "Read CONFIGURATION's global native MCP server definitions."
  (mcp-configuration-read-path (configuration-mcp-path configuration)))


;;;; -- Layered Server Registry --

(defvar *mcp-server-registry-lock*
  (make-lock "Autolith MCP server registry")
  "The lock protecting live MCP server registration layers.")

(defvar *mcp-server-registrations* nil
  "Ordered source-attributed MCP server registration layers.")

(-> mcp--current-registration-source () keyword)
(defun mcp--current-registration-source ()
  "Return the registration source appropriate to the current load context."
  *extension-registration-source*)

(-> mcp--registration-source-rank (keyword) integer)
(defun mcp--registration-source-rank (source)
  "Return SOURCE's explicit MCP precedence rank or reject SOURCE."
  (or (rest (assoc source *mcp-registration-source-precedence*))
      (mcp-configuration--error
       (format nil "Unsupported MCP registration source ~S." source))))

(-> mcp--validate-registration-list (list) list)
(defun mcp--validate-registration-list (registrations)
  "Return REGISTRATIONS after validating every MCP registration layer."
  (unless (proper-list-p registrations)
    (mcp-configuration--error
     "The MCP server registry snapshot must be a proper list."))
  (let ((seen         (make-hash-table :test #'equal))
        (server-names (make-hash-table :test #'equal)))
    (dolist (registration registrations)
      (unless (typep registration 'mcp-server-registration)
        (mcp-configuration--error
         "The MCP server registry contains an invalid registration layer."))
      (let* ((source (mcp-server-registration-source registration))
             (configuration
               (mcp-server-registration-configuration registration)))
        (unless (and (keywordp source)
                     (typep configuration 'mcp-server-configuration))
          (mcp-configuration--error
           "The MCP server registry contains an invalid registration layer."))
        (mcp--registration-source-rank source)
        (let ((name (mcp-server-configuration-name configuration)))
          (unless
              (mcp-configuration--bounded-string-p
               name *mcp-server-name-maximum-characters*)
            (mcp-configuration--error
             "The MCP server registry contains an invalid server name."))
          (let ((key (cons source name)))
            (when (gethash key seen)
              (mcp-configuration--error
               "The MCP server registry contains a duplicate source and server layer."
               :server-name (rest key)))
            (setf (gethash key seen) t)
            (setf (gethash name server-names) t)))))
    (when (> (hash-table-count server-names)
             *mcp-configuration-maximum-servers*)
      (mcp-configuration--error
       (format nil "The MCP server registry exceeds the limit of ~D effective servers."
               *mcp-configuration-maximum-servers*))))
  registrations)

(-> mcp--effective-registrations (list) list)
(defun mcp--effective-registrations (registrations)
  "Return each server's highest-precedence registration layer."
  (layered-registry-effective
   registrations
   (lambda (registration)
     (mcp-server-configuration-name
      (mcp-server-registration-configuration registration)))
   (lambda (registration)
     (mcp--registration-source-rank
      (mcp-server-registration-source registration)))))

(-> mcp-server-registrations () list)
(defun mcp-server-registrations ()
  "Return an ordered snapshot of effective MCP server registrations."
  (with-extension-registry-transaction
    (with-lock-held (*mcp-server-registry-lock*)
      (copy-list
       (mcp--effective-registrations *mcp-server-registrations*)))))

(-> register-mcp-server
    ((or list mcp-server-configuration) &key (:source keyword))
    mcp-server-configuration)
(defun register-mcp-server
    (definition &key (source (mcp--current-registration-source)))
  "Register one native MCP server DEFINITION in SOURCE and return it.

DEFINITION may be an MCP-SERVER-CONFIGURATION or the same strict property list
accepted in mcp.sexp. The same source and case-sensitive server name replace
their prior layer without destroying shadowed lower layers."
  (unless (keywordp source)
    (mcp-configuration--error
     "An MCP registration source must be a keyword."))
  (mcp--registration-source-rank source)
  (let* ((configuration
           (etypecase definition
             (mcp-server-configuration
              definition)
             (list
              (mcp-configuration--server definition))))
         (replacement
           (make-instance 'mcp-server-registration
                          :configuration configuration
                          :source        source)))
    (with-extension-registry-transaction
      (with-lock-held (*mcp-server-registry-lock*)
        (let ((candidate
                (layered-registry-replace
                 *mcp-server-registrations* replacement
                 :key-function
                 (lambda (registration)
                   (mcp-server-configuration-name
                    (mcp-server-registration-configuration registration)))
                 :source-function #'mcp-server-registration-source
                 :key-test        #'string=
                 :source          source)))
          (mcp--validate-registration-list candidate)
          (setf *mcp-server-registrations* candidate))))
    configuration))

(-> mcp--registry-snapshot () list)
(defun mcp--registry-snapshot ()
  "Return an exact ordered snapshot of MCP registration layers."
  (with-extension-registry-transaction
    (with-lock-held (*mcp-server-registry-lock*)
      (copy-list *mcp-server-registrations*))))

(-> mcp--registry-restore (list) null)
(defun mcp--registry-restore (snapshot)
  "Restore exact MCP registration SNAPSHOT after validating it."
  (mcp--validate-registration-list snapshot)
  (with-extension-registry-transaction
    (with-lock-held (*mcp-server-registry-lock*)
      (setf *mcp-server-registrations* (copy-list snapshot))))
  nil)

(-> mcp--remove-registration-source (keyword) null)
(defun mcp--remove-registration-source (source)
  "Remove every MCP registration layer attributed to SOURCE."
  (unless (keywordp source)
    (mcp-configuration--error
     "An MCP registration source must be a keyword."))
  (mcp--registration-source-rank source)
  (with-extension-registry-transaction
    (with-lock-held (*mcp-server-registry-lock*)
      (setf *mcp-server-registrations*
            (layered-registry-remove-source
             *mcp-server-registrations* source
             #'mcp-server-registration-source))))
  nil)
