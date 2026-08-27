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

(defparameter *mcp-stdio-command-maximum-characters* 4096
  "The maximum character length of an MCP standard-input command.")

(defparameter *mcp-stdio-argument-maximum-characters* 8192
  "The maximum character length of one MCP standard-input argument.")

(defparameter *mcp-stdio-maximum-arguments* 128
  "The maximum number of arguments for one MCP standard-input server.")

(defparameter *mcp-stdio-directory-maximum-characters* 4096
  "The maximum character length of an MCP standard-input directory.")

(defparameter *mcp-environment-name-maximum-characters* 255
  "The maximum character length of an MCP environment variable name.")

(defparameter *mcp-stdio-maximum-environment-bindings* 64
  "The maximum number of environment bindings for one MCP standard-input server.")

(defparameter *mcp-http-url-maximum-characters* 8192
  "The maximum character length of one MCP Streamable HTTP URL.")

(defparameter *mcp-http-header-name-maximum-characters* 255
  "The maximum character length of an MCP HTTP header name.")

(defparameter *mcp-http-maximum-header-bindings* 64
  "The maximum number of header bindings for one MCP Streamable HTTP server.")

(defparameter *mcp-child-tool-name-maximum-characters* 256
  "The maximum character length of one raw MCP child tool name.")

(defparameter *mcp-maximum-child-tools* 128
  "The maximum number of tools one MCP server may grant to child agents.")

(defparameter *mcp-maximum-trusted-read-only-tools* 128
  "The maximum exact tool names trusted for read-only MCP annotations.")

(defparameter *mcp-default-startup-timeout-seconds* 15
  "The default deadline for MCP initialization and discovery.")

(defparameter *mcp-default-tool-timeout-seconds* 60
  "The default deadline for one MCP tool call.")

(defparameter *mcp-maximum-timeout-seconds* 3600
  "The maximum configurable MCP transport or operation deadline.")

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
    :initarg :pathname
    :initform nil
    :reader mcp-configuration-error-pathname
    :type (option pathname)
    :documentation "The native MCP configuration file involved, when any.")
   (server-name
    :initarg :server-name
    :initform nil
    :reader mcp-configuration-error-server-name
    :type (option string)
    :documentation "The server definition involved, when known.")
   (field
    :initarg :field
    :initform nil
    :reader mcp-configuration-error-field
    :type (option keyword)
    :documentation "The invalid native field, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader mcp-configuration-error-cause
    :type t
    :documentation "The underlying reader or validation failure, when any."))
  (:documentation "A native Autolith MCP configuration is malformed."))


;;;; -- Immutable Configuration Objects --

(defclass mcp-environment-binding ()
  ((target
    :initarg :target
    :reader mcp-environment-binding-target
    :type non-empty-string
    :documentation "The header or child-process environment name being set.")
   (source
    :initarg :source
    :reader mcp-environment-binding-source
    :type non-empty-string
    :documentation "The parent environment variable read only when needed."))
  (:documentation "One credential-safe reference to a process environment value."))

(defclass mcp-transport-configuration ()
  ()
  (:documentation "The immutable native configuration of one MCP transport."))

(defclass mcp-stdio-transport-configuration (mcp-transport-configuration)
  ((command
    :initarg :command
    :reader mcp-stdio-configuration-command
    :type non-empty-string
    :documentation "The executable used to start the MCP server.")
   (arguments
    :initarg :arguments
    :initform nil
    :reader mcp-stdio-configuration-arguments
    :type list
    :documentation "The exact argument strings following the executable.")
   (directory
    :initarg :directory
    :initform ':workspace
    :reader mcp-stdio-configuration-directory
    :type t
    :documentation "The :WORKSPACE marker or configured server directory.")
   (environment-bindings
    :initarg :environment-bindings
    :initform nil
    :reader mcp-stdio-configuration-environment-bindings
    :type list
    :documentation "Environment values resolved from parent variables at launch."))
  (:documentation "A native standard-input and standard-output MCP transport."))

(defclass mcp-http-transport-configuration (mcp-transport-configuration)
  ((url
    :initarg :url
    :reader mcp-http-configuration-url
    :type non-empty-string
    :documentation "The non-credential Streamable HTTP endpoint.")
   (header-bindings
    :initarg :header-bindings
    :initform nil
    :reader mcp-http-configuration-header-bindings
    :type list
    :documentation "HTTP headers resolved from environment variables per request.")
   (connect-timeout-seconds
    :initarg :connect-timeout-seconds
    :initform 10
    :reader mcp-http-configuration-connect-timeout-seconds
    :type real
    :documentation "The bounded HTTP connection deadline."))
  (:documentation "A native MCP Streamable HTTP transport configuration."))

(defclass mcp-server-configuration ()
  ((name
    :initarg :name
    :reader mcp-server-configuration-name
    :type non-empty-string
    :documentation "The user-facing, case-sensitive server identifier.")
   (transport
    :initarg :transport
    :reader mcp-server-configuration-transport
    :type mcp-transport-configuration
    :documentation "The transport used to reach this server.")
   (required-p
    :initarg :required-p
    :initform nil
    :reader mcp-server-configuration-required-p
    :type boolean
    :documentation "Whether discovery failure prevents Autolith startup.")
   (startup-timeout-seconds
    :initarg :startup-timeout-seconds
    :initform *mcp-default-startup-timeout-seconds*
    :reader mcp-server-configuration-startup-timeout-seconds
    :type real
    :documentation "The server initialization and discovery deadline.")
   (tool-timeout-seconds
    :initarg :tool-timeout-seconds
    :initform *mcp-default-tool-timeout-seconds*
    :reader mcp-server-configuration-tool-timeout-seconds
    :type real
    :documentation "The default deadline for one server tool call.")
   (approval-policy
    :initarg :approval-policy
    :initform ':prompt
    :reader mcp-server-configuration-approval-policy
    :type keyword
    :documentation "The policy deciding which MCP tool calls need approval.")
   (trusted-read-only-tools
    :initarg :trusted-read-only-tools
    :initform nil
    :reader mcp-server-configuration-trusted-read-only-tools
    :type list
    :documentation
    "Exact raw tools whose read-only annotations the user explicitly trusts.")
   (child-tools
    :initarg :child-tools
    :initform nil
    :reader mcp-server-configuration-child-tools
    :type list
    :documentation "Exact raw MCP tool names explicitly granted to child agents."))
  (:documentation "One complete native Autolith MCP server definition."))

(defclass mcp-server-registration ()
  ((configuration
    :initarg :configuration
    :reader mcp-server-registration-configuration
    :type mcp-server-configuration
    :documentation "The immutable registered server configuration.")
   (source
    :initarg :source
    :reader mcp-server-registration-source
    :type keyword
    :documentation
    "The tracked, site-config, site, config, directory, user, or runtime registration layer."))
  (:documentation "One source-attributed layer in the MCP server registry."))


;;;; -- Native Form Validation --

(-> configuration-mcp-path (configuration) pathname)
(defun configuration-mcp-path (configuration)
  "Return CONFIGURATION's native versioned MCP file."
  (merge-pathnames "mcp.sexp" (configuration-config-root configuration)))

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
         :message message
         :pathname pathname
         :server-name server-name
         :field field
         :cause cause))

(-> mcp-configuration--proper-list-p (t) boolean)
(defun mcp-configuration--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (or (null value)
      (and (listp value)
           (handler-case
               (integerp (list-length value))
             (type-error ()
               nil)))))

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
   :label "Native MCP configuration"
   :keywords *mcp-configuration-native-keywords*
   :maximum-depth *mcp-configuration-maximum-depth*
   :maximum-nodes *mcp-configuration-maximum-nodes*
   :improper-lists-permitted-p t
   :allowed-atom-predicate #'mcp-configuration--native-value-p))

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
  (unless (mcp-configuration--proper-list-p value)
    (mcp-configuration--error
     "An MCP native object must be a proper property list."
     :pathname pathname
     :server-name server-name))
  (unless (evenp (length value))
    (mcp-configuration--error
     "An MCP native object has a property without a value."
     :pathname pathname
     :server-name server-name))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for tail on value by #'cddr
          for key = (first tail)
          do
             (unless (keywordp key)
               (mcp-configuration--error
                (format nil "MCP configuration key ~S is not a keyword." key)
                :pathname pathname
                :server-name server-name))
             (unless (member key allowed-keys)
               (mcp-configuration--error
                (format nil "Unknown MCP configuration key ~S." key)
                :pathname pathname
                :server-name server-name
                :field key))
             (when (gethash key seen)
               (mcp-configuration--error
                (format nil "Duplicate MCP configuration key ~S." key)
                :pathname pathname
                :server-name server-name
                :field key))
             (setf (gethash key seen) t)))
  value)

(-> mcp-configuration--property
    (list keyword &key (:required-p boolean)
                  (:pathname (option pathname))
                  (:server-name (option string)))
    t)
(defun mcp-configuration--property
    (properties key &key required-p pathname server-name)
  "Return KEY from PROPERTIES and reject an absent required value."
  (loop for (candidate value) on properties by #'cddr
        when (eq candidate key)
          do (return-from mcp-configuration--property value))
  (when required-p
    (mcp-configuration--error
     (format nil "MCP configuration is missing required key ~S." key)
     :pathname pathname
     :server-name server-name
     :field key))
  nil)

(-> mcp-configuration--property-present-p (list keyword) boolean)
(defun mcp-configuration--property-present-p (properties key)
  "Return true when PROPERTIES explicitly contains KEY."
  (loop for tail on properties by #'cddr
        thereis (eq (first tail) key)))

(-> mcp-configuration--environment-name-p (t) boolean)
(defun mcp-configuration--environment-name-p (value)
  "Return true when VALUE is a portable POSIX environment name."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) *mcp-environment-name-maximum-characters*)
       (let ((first-character (char value 0)))
         (or (and (<= (char-code first-character) 127)
                  (alpha-char-p first-character))
             (char= first-character #\_)))
       (loop for character across value
             always (or (and (<= (char-code character) 127)
                             (alphanumericp character))
                        (char= character #\_)))))

(-> mcp-configuration--http-header-name-p (t) boolean)
(defun mcp-configuration--http-header-name-p (value)
  "Return true when VALUE is a non-reserved HTTP token header name."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) *mcp-http-header-name-maximum-characters*)
       (loop for character across value
             always
             (or (and (<= (char-code character) 127)
                      (alphanumericp character))
                 (find character "!#$%&'*+-.^_`|~" :test #'char=)))
       (not (member value
                    '("content-type" "accept" "mcp-session-id"
                      "mcp-protocol-version" "last-event-id")
                    :test #'string-equal))))

(-> mcp-configuration--binding
    (t &key (:header-p boolean)
            (:pathname (option pathname))
            (:server-name (option string)))
    mcp-environment-binding)
(defun mcp-configuration--binding
    (form &key header-p pathname server-name)
  "Parse one environment-backed process or HTTP binding FORM."
  (unless (and (mcp-configuration--proper-list-p form)
               (= (length form) 3)
               (eq (second form) :environment))
    (mcp-configuration--error
     "An MCP environment binding must be (TARGET :ENVIRONMENT SOURCE)."
     :pathname pathname
     :server-name server-name))
  (let ((target (first form))
        (source (third form)))
    (unless (if header-p
                (mcp-configuration--http-header-name-p target)
                (mcp-configuration--environment-name-p target))
      (mcp-configuration--error
       (format nil "Invalid MCP ~A name."
               (if header-p "HTTP header" "environment"))
       :pathname pathname
       :server-name server-name))
    (unless (mcp-configuration--environment-name-p source)
      (mcp-configuration--error
       "Invalid MCP source environment name."
       :pathname pathname
       :server-name server-name))
    (make-instance 'mcp-environment-binding
                   :target (copy-seq target)
                   :source (copy-seq source))))

(-> mcp-configuration--bindings
    (t &key (:header-p boolean)
            (:pathname (option pathname))
            (:server-name (option string)))
    list)
(defun mcp-configuration--bindings
    (forms &key header-p pathname server-name)
  "Validate and copy unique environment-backed binding FORMS."
  (unless (mcp-configuration--proper-list-p forms)
    (mcp-configuration--error
     "MCP environment bindings must be a proper list."
     :pathname pathname
     :server-name server-name))
  (let ((maximum
          (if header-p
              *mcp-http-maximum-header-bindings*
              *mcp-stdio-maximum-environment-bindings*)))
    (when (> (length forms) maximum)
      (mcp-configuration--error
       (format nil "MCP ~A bindings exceed the limit of ~D."
               (if header-p "HTTP header" "environment")
               maximum)
       :pathname pathname
       :server-name server-name
       :field (if header-p :headers :environment))))
  (let ((bindings
          (mapcar
           (lambda (form)
             (mcp-configuration--binding
              form
              :header-p header-p
              :pathname pathname
              :server-name server-name))
           forms))
        (seen (make-hash-table :test #'equalp)))
    (dolist (binding bindings)
      (let ((target (mcp-environment-binding-target binding)))
        (when (gethash target seen)
          (mcp-configuration--error
           (format nil "Duplicate MCP binding target ~S." target)
           :pathname pathname
           :server-name server-name))
        (setf (gethash target seen) t)))
    bindings))

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
     :pathname pathname
     :server-name server-name
     :field field))
  value)

(-> mcp-configuration--stdio
    (list &key (:pathname (option pathname))
               (:server-name (option string)))
    mcp-stdio-transport-configuration)
(defun mcp-configuration--stdio (form &key pathname server-name)
  "Parse one native standard-input and standard-output transport FORM."
  (mcp-configuration--validate-plist
   form
   '(:type :command :arguments :directory :environment)
   :pathname pathname
   :server-name server-name)
  (let* ((command
           (mcp-configuration--property
            form :command
            :required-p t
            :pathname pathname
            :server-name server-name))
         (arguments
           (mcp-configuration--property form :arguments))
         (directory
           (if (mcp-configuration--property-present-p form :directory)
               (mcp-configuration--property form :directory)
               :workspace))
         (environment
           (mcp-configuration--property form :environment)))
    (unless
        (mcp-configuration--bounded-string-p
         command *mcp-stdio-command-maximum-characters*)
      (mcp-configuration--error
       "An MCP stdio command must be a bounded non-empty string."
       :pathname pathname
       :server-name server-name
       :field ':command))
    (unless (and (mcp-configuration--proper-list-p arguments)
                 (<= (length arguments) *mcp-stdio-maximum-arguments*)
                 (every
                  (lambda (argument)
                    (mcp-configuration--bounded-string-p
                     argument
                     *mcp-stdio-argument-maximum-characters*
                     :empty-p t))
                  arguments))
      (mcp-configuration--error
       "MCP stdio arguments must be a bounded proper list of bounded strings."
       :pathname pathname
       :server-name server-name
       :field ':arguments))
    (unless (or (eq directory :workspace)
                (mcp-configuration--bounded-string-p
                 directory
                 *mcp-stdio-directory-maximum-characters*))
      (mcp-configuration--error
       "An MCP stdio directory must be :WORKSPACE or a bounded non-empty string."
       :pathname pathname
       :server-name server-name
       :field ':directory))
    (make-instance
     'mcp-stdio-transport-configuration
     :command (copy-seq command)
     :arguments (mapcar #'copy-seq arguments)
     :directory (if (stringp directory) (copy-seq directory) directory)
     :environment-bindings
     (mcp-configuration--bindings
      environment
      :pathname pathname
      :server-name server-name))))

(-> mcp-configuration--split-string (string character) list)
(defun mcp-configuration--split-string (value separator)
  "Split VALUE at every SEPARATOR while preserving empty components."
  (loop with start = 0
        for position = (position separator value :start start)
        collect (subseq value start position)
        while position
        do (setf start (1+ position))))

(-> mcp-configuration--domain-host-p (string) boolean)
(defun mcp-configuration--domain-host-p (host)
  "Return true when HOST is a bounded ASCII DNS name."
  (let ((name
          (if (and (plusp (length host))
                   (char= (char host (1- (length host))) #\.))
              (subseq host 0 (1- (length host)))
              host)))
    (and (plusp (length name))
         (<= (length name) 253)
         (every
          (lambda (label)
            (and (plusp (length label))
                 (<= (length label) 63)
                 (let ((first-character (char label 0))
                       (last-character (char label (1- (length label))))
                       (ascii-alphanumeric-p
                         (lambda (character)
                           (and (<= (char-code character) 127)
                                (alphanumericp character)))))
                   (and (funcall ascii-alphanumeric-p first-character)
                        (funcall ascii-alphanumeric-p last-character)
                        (loop for character across label
                              always
                              (or
                               (funcall ascii-alphanumeric-p character)
                               (char= character #\-)))))))
          (mcp-configuration--split-string name #\.)))))

(-> mcp-configuration--http-host-p (string) boolean)
(defun mcp-configuration--http-host-p (host)
  "Return true when HOST is a validated DNS, IPv4, or bracketed IPv6 host."
  (cond
    ((ip-addr-p host)
     t)
    ((find #\: host)
     nil)
    ((loop for character across host
           always (or (digit-char-p character 10)
                      (char= character #\.)))
     nil)
    (t
     (mcp-configuration--domain-host-p host))))

(-> mcp-configuration--loopback-host-p (string) boolean)
(defun mcp-configuration--loopback-host-p (host)
  "Return true only when HOST denotes an unambiguous loopback address."
  (or (string-equal host "localhost")
      (string-equal host "localhost.")
      (and
       (ipv4-addr-p host)
       (let ((first-dot (position #\. host)))
         (and first-dot
              (string= (subseq host 0 first-dot) "127"))))
      (and
       (ipv6-addr-p host)
       (ip-addr= host "[::1]"))))

(-> mcp-configuration--url-authority-port-syntax-p (string) boolean)
(defun mcp-configuration--url-authority-port-syntax-p (url)
  "Return true when URL's authority has a syntactically valid optional port."
  (let ((scheme-end (search "://" url)))
    (unless scheme-end
      (return-from mcp-configuration--url-authority-port-syntax-p nil))
    (let* ((authority-start (+ scheme-end 3))
           (authority-end
             (or (position-if
                  (lambda (character)
                    (member character '(#\/ #\? #\#) :test #'char=))
                  url
                  :start authority-start)
                 (length url)))
           (authority (subseq url authority-start authority-end)))
      (unless (plusp (length authority))
        (return-from mcp-configuration--url-authority-port-syntax-p nil))
      (labels ((decimal-port-p (value)
                 "Return true when VALUE is a non-empty decimal port."
                 (and (plusp (length value))
                      (loop for character across value
                            always
                            (and (<= (char-code character) 127)
                                 (digit-char-p character 10))))))
        (if (char= (char authority 0) #\[)
            (let ((closing-bracket (position #\] authority)))
              (and closing-bracket
                   (let ((tail (subseq authority (1+ closing-bracket))))
                     (or (zerop (length tail))
                         (and (char= (char tail 0) #\:)
                              (decimal-port-p (subseq tail 1)))))))
            (let ((colon (position #\: authority)))
              (or (null colon)
                  (and (= colon (position #\: authority :from-end t))
                       (decimal-port-p
                        (subseq authority (1+ colon)))))))))))

(-> mcp-configuration--validate-http-url
    (string &key (:pathname (option pathname))
                 (:server-name (option string)))
    string)
(defun mcp-configuration--validate-http-url
    (url &key pathname server-name)
  "Return URL after strict Streamable HTTP endpoint validation."
  (unless
      (mcp-configuration--bounded-string-p
       url *mcp-http-url-maximum-characters*)
    (mcp-configuration--error
     "An MCP Streamable HTTP URL must be a bounded non-empty string."
     :pathname pathname
     :server-name server-name
     :field ':url))
  (handler-case
      (let* ((uri (uri url))
             (scheme (uri-scheme uri))
             (host (uri-host uri))
             (port (uri-port uri)))
        (unless (and (member scheme '("http" "https")
                             :test #'string-equal)
                     (mcp-configuration--url-authority-port-syntax-p url)
                     (stringp host)
                     (mcp-configuration--http-host-p host)
                     (integerp port)
                     (<= 1 port 65535))
          (mcp-configuration--error
           "An MCP HTTP URL must contain a valid HTTP host and port."
           :pathname pathname
           :server-name server-name
           :field ':url))
        (unless (or (string-equal scheme "https")
                    (and (string-equal scheme "http")
                         (mcp-configuration--loopback-host-p host)))
          (mcp-configuration--error
           "An MCP HTTP URL must use HTTPS unless its host is loopback."
           :pathname pathname
           :server-name server-name
           :field ':url)))
    (mcp-configuration-error (condition)
      (error condition))
    (error (cause)
      (declare (ignore cause))
      (mcp-configuration--error
       "Invalid MCP Streamable HTTP URL."
       :pathname pathname
       :server-name server-name
       :field ':url)))
  url)

(-> mcp-configuration--http
    (list &key (:pathname (option pathname))
               (:server-name (option string)))
    mcp-http-transport-configuration)
(defun mcp-configuration--http (form &key pathname server-name)
  "Parse one native Streamable HTTP transport FORM."
  (mcp-configuration--validate-plist
   form
   '(:type :url :headers :connect-timeout-seconds)
   :pathname pathname
   :server-name server-name)
  (let* ((url
           (mcp-configuration--property
            form :url
            :required-p t
            :pathname pathname
            :server-name server-name))
         (headers
           (mcp-configuration--property form :headers))
         (connect-timeout
           (if (mcp-configuration--property-present-p
                form :connect-timeout-seconds)
               (mcp-configuration--property
                form :connect-timeout-seconds)
               10)))
    (mcp-configuration--validate-http-url
     url
     :pathname pathname
     :server-name server-name)
    (make-instance
     'mcp-http-transport-configuration
     :url (copy-seq url)
     :header-bindings
     (mcp-configuration--bindings
      headers
      :header-p t
      :pathname pathname
      :server-name server-name)
     :connect-timeout-seconds
     (mcp-configuration--positive-timeout
      connect-timeout
      :connect-timeout-seconds
      :pathname pathname
      :server-name server-name))))

(-> mcp-configuration--transport
    (t &key (:pathname (option pathname))
             (:server-name (option string)))
    mcp-transport-configuration)
(defun mcp-configuration--transport (form &key pathname server-name)
  "Parse one native MCP transport FORM."
  (unless (mcp-configuration--proper-list-p form)
    (mcp-configuration--error
     "An MCP transport must be a native property list."
     :pathname pathname
     :server-name server-name
     :field ':transport))
  (let ((type
          (mcp-configuration--property
           form :type
           :required-p t
           :pathname pathname
           :server-name server-name)))
    (case type
      (:stdio
       (mcp-configuration--stdio
        form :pathname pathname :server-name server-name))
      (:http
       (mcp-configuration--http
        form :pathname pathname :server-name server-name))
      (otherwise
       (mcp-configuration--error
        "Unsupported MCP transport type."
        :pathname pathname
        :server-name server-name
        :field ':type)))))

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
     :field ':name))
  (unless (or (null required-p) (eq required-p t))
    (mcp-configuration--error
     "MCP :REQUIRED-P must be exactly T or NIL."
     :pathname pathname
     :server-name name
     :field ':required-p))
  (unless (member approval *mcp-approval-policies*)
    (mcp-configuration--error
     "Unsupported MCP approval policy."
     :pathname pathname
     :server-name name
     :field ':approval))
  (unless
      (and
       (mcp-configuration--proper-list-p trusted-read-only-tools)
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
     :pathname pathname
     :server-name name
     :field ':trusted-read-only-tools))
  (when (and trusted-read-only-tools
             (not (eq approval :read-only)))
    (mcp-configuration--error
     "MCP :TRUSTED-READ-ONLY-TOOLS requires :APPROVAL :READ-ONLY."
     :pathname pathname
     :server-name name
     :field ':trusted-read-only-tools))
  (unless (and (mcp-configuration--proper-list-p child-tools)
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
     :pathname pathname
     :server-name name
     :field ':child-tools))
  (make-instance
   'mcp-server-configuration
   :name (copy-seq name)
   :transport
   (mcp-configuration--transport
    transport
    :pathname pathname
    :server-name name)
   :required-p (and required-p t)
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
   :child-tools (mapcar #'copy-seq child-tools)))

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
     :name name
     :transport
     (mcp-configuration--property
      form :transport
      :required-p t
      :pathname pathname
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
  "Read one regular PATHNAME as bounded UTF-8 without blocking or racing."
  (handler-case
      (let ((descriptor nil))
        (unwind-protect
             (progn
               (setf descriptor
                     (sb-posix:open
                      (namestring pathname)
                      (logior sb-posix:o-rdonly
                              sb-posix:o-nonblock)))
               (unless
                   (sb-posix:s-isreg
                    (sb-posix:stat-mode
                     (sb-posix:fstat descriptor)))
                 (mcp-configuration--error
                  "The native MCP configuration must be a regular file."
                  :pathname pathname))
               (let* ((buffer
                        (make-array
                         (1+ *mcp-configuration-maximum-bytes*)
                         :element-type '(unsigned-byte 8)))
                      (count
                        (loop with offset = 0
                              while (< offset (length buffer))
                              for read-count =
                                (sb-sys:with-pinned-objects (buffer)
                                  (sb-posix:read
                                   descriptor
                                   (sb-sys:sap+
                                    (sb-sys:vector-sap buffer)
                                    offset)
                                   (- (length buffer) offset)))
                              do
                                 (when (zerop read-count)
                                   (return offset))
                                 (incf offset read-count)
                              finally (return offset))))
                 (when (> count *mcp-configuration-maximum-bytes*)
                   (mcp-configuration--error
                    "The native MCP configuration exceeds its byte bound."
                    :pathname pathname))
                 (sb-ext:octets-to-string
                  buffer
                  :external-format ':utf-8
                  :start 0
                  :end count)))
          (when descriptor
            (sb-posix:close descriptor))))
    (mcp-configuration-error (condition)
      (error condition))
    (serious-condition (cause)
      (mcp-configuration--error
       (format nil "Could not read native MCP configuration at ~A: ~A"
               pathname cause)
       :pathname pathname
       :cause cause))))

(-> mcp-configuration--source-present-p
    (pathname &key (:description string))
    boolean)
(defun mcp-configuration--source-present-p
    (pathname &key (description "native MCP configuration"))
  "Return true when PATHNAME resolves to a regular file named by DESCRIPTION."
  (handler-case
      (let ((status (sb-posix:stat (namestring pathname))))
        (unless (sb-posix:s-isreg (sb-posix:stat-mode status))
          (mcp-configuration--error
           (format nil "The ~A must be a regular file." description)
           :pathname pathname))
        t)
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          (handler-case
              (progn
                (sb-posix:lstat (namestring pathname))
                (mcp-configuration--error
                 (format nil "The ~A link has no regular target." description)
                 :pathname pathname
                 :cause condition))
            (sb-posix:syscall-error (link-condition)
              (if (= (sb-posix:syscall-errno link-condition)
                     sb-posix:enoent)
                  nil
                  (mcp-configuration--error
                   (format nil
                           "Could not inspect ~A at ~A: ~A"
                           description
                           pathname
                           link-condition)
                   :pathname pathname
                   :cause link-condition))))
          (mcp-configuration--error
           (format nil "Could not inspect ~A at ~A: ~A"
                   description pathname condition)
           :pathname pathname
           :cause condition)))))

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
       :field ':version))
    (let ((servers
            (mcp-configuration--property
             form :servers :required-p t :pathname pathname)))
      (unless (mcp-configuration--proper-list-p servers)
        (mcp-configuration--error
         "MCP :SERVERS must be a proper list."
         :pathname pathname
         :field ':servers))
      (when (> (length servers) *mcp-configuration-maximum-servers*)
        (mcp-configuration--error
         (format nil "MCP :SERVERS exceeds the limit of ~D."
                 *mcp-configuration-maximum-servers*)
         :pathname pathname
         :field ':servers))
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
               :pathname pathname
               :server-name name
               :field ':name))
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
  (unless (mcp-configuration--proper-list-p registrations)
    (mcp-configuration--error
     "The MCP server registry snapshot must be a proper list."))
  (let ((seen (make-hash-table :test #'equal))
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
  (let ((order nil)
        (seen (make-hash-table :test #'equal))
        (winners (make-hash-table :test #'equal)))
    (dolist (registration registrations)
      (let ((name
              (mcp-server-configuration-name
               (mcp-server-registration-configuration registration))))
        (unless (gethash name seen)
          (setf (gethash name seen) t)
          (setf order (nconc order (list name))))
        (let ((winner (gethash name winners)))
          (when (or (null winner)
                    (>=
                     (mcp--registration-source-rank
                      (mcp-server-registration-source registration))
                     (mcp--registration-source-rank
                      (mcp-server-registration-source winner))))
            (setf (gethash name winners) registration)))))
    (mapcar (lambda (name) (gethash name winners)) order)))

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
             (mcp-server-configuration definition)
             (list (mcp-configuration--server definition))))
         (name (mcp-server-configuration-name configuration))
         (replacement
           (make-instance 'mcp-server-registration
                          :configuration configuration
                          :source source)))
    (with-extension-registry-transaction
      (with-lock-held (*mcp-server-registry-lock*)
        (let ((position
                (position-if
                 (lambda (registration)
                   (and
                    (eq source (mcp-server-registration-source registration))
                    (string=
                     name
                     (mcp-server-configuration-name
                      (mcp-server-registration-configuration registration)))))
                 *mcp-server-registrations*)))
          (let ((candidate
                  (if position
                      (append
                       (subseq *mcp-server-registrations* 0 position)
                       (list replacement)
                       (nthcdr (1+ position) *mcp-server-registrations*))
                      (append *mcp-server-registrations*
                              (list replacement)))))
            (mcp--validate-registration-list candidate)
            (setf *mcp-server-registrations* candidate)))))
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
            (remove source
                    *mcp-server-registrations*
                    :key #'mcp-server-registration-source))))
  nil)
