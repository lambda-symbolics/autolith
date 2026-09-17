(in-package #:autolith)

;;;; -- Native LSP Configuration --

(defparameter *lsp-configuration-version* 1
  "The native LSP configuration version accepted by Autolith.")
(defparameter *lsp-configuration-maximum-bytes* (* 256 1024)
  "The maximum byte length of lsp.sexp.")
(defparameter *lsp-configuration-maximum-servers* 32
  "The maximum number of configured LSP servers.")
(defparameter *lsp-configuration-maximum-timeout-seconds* 120
  "The maximum LSP server startup/request timeout.")
(defparameter *lsp-configuration-maximum-string-characters* 8192
  "The maximum length of one LSP configuration string.")
(defparameter *lsp-configuration-maximum-list-elements* 128
  "The maximum number of entries in one LSP list field.")

(define-condition lsp-configuration-error (configuration-error)
  ((pathname :initarg :pathname :initform nil :reader lsp-configuration-error-pathname
             :documentation "The user configuration file involved.")
   (server-name :initarg :server-name :initform nil :reader lsp-configuration-error-server-name
                :documentation "The server definition involved, when known.")
   (field :initarg :field :initform nil :reader lsp-configuration-error-field
          :documentation "The invalid configuration field.")
   (cause :initarg :cause :initform nil :reader lsp-configuration-error-cause
          :documentation "The underlying parser or filesystem condition."))
  (:documentation "A malformed native LSP configuration."))

(defclass lsp-server-configuration ()
  ((name :initarg :name :reader lsp-server-configuration-name :type string
         :documentation "Unique user-selected server name.")
   (command :initarg :command :reader lsp-server-configuration-command :type string
            :documentation "Trusted executable, resolved on the inherited PATH.")
   (arguments :initarg :arguments :initform nil :reader lsp-server-configuration-arguments :type list
              :documentation "Literal argv strings, without shell expansion.")
   (extensions :initarg :extensions :initform nil :reader lsp-server-configuration-extensions :type list
               :documentation "Filename suffixes handled by this server.")
   (language-id :initarg :language-id :reader lsp-server-configuration-language-id :type string
                :documentation "LSP language identifier sent in didOpen.")
   (root-markers :initarg :root-markers :initform nil :reader lsp-server-configuration-root-markers :type list
                 :documentation "Literal marker filenames used for nearest project-root selection.")
   (initialization-options :initarg :initialization-options :initform nil
                           :reader lsp-server-configuration-initialization-options :type (option hash-table)
                           :documentation "Decoded initializationOptions object, or JSON null.")
   (settings :initarg :settings :initform (json-object) :reader lsp-server-configuration-settings :type hash-table
             :documentation "Decoded workspace configuration object.")
   (timeout-seconds :initarg :timeout-seconds :initform 30
                    :reader lsp-server-configuration-timeout-seconds :type integer
                    :documentation "Startup and request deadline in seconds.")
   (disabled-p :initarg :disabled-p :initform nil :reader lsp-server-configuration-disabled-p :type boolean
               :documentation "Whether this entry is excluded from automatic selection."))
  (:documentation "One strict declarative LSP server configuration."))

(-> lsp-configuration-path (configuration) pathname)
(defun lsp-configuration-path (configuration)
  "Return CONFIGURATION's user-controlled native LSP configuration pathname."
  (merge-pathnames "lsp.sexp" (configuration-config-root configuration)))

(-> lsp-configuration--error (string &key (:pathname t) (:server-name t) (:field t) (:cause t)) nil)
(defun lsp-configuration--error (message &key pathname server-name field cause)
  "Signal a structured configuration error with its source context."
  (error 'lsp-configuration-error :message message :pathname pathname
         :server-name server-name :field field :cause cause))

(-> lsp-configuration--bounded-string-p (t &key (:empty-p boolean)) boolean)
(defun lsp-configuration--bounded-string-p (value &key empty-p)
  "Return true for bounded strings without embedded NUL bytes."
  (and (stringp value) (not (find #\Null value))
       (<= (length value) *lsp-configuration-maximum-string-characters*)
       (or empty-p (plusp (length value)))))

(-> lsp-configuration--proper-list-p (t) boolean)
(defun lsp-configuration--proper-list-p (value)
  "Return true for a finite proper list, including NIL."
  (and (listp value)
       (handler-case (not (null (list-length value))) (type-error () nil))))

(-> lsp-configuration--list-p (t function) boolean)
(defun lsp-configuration--list-p (value predicate)
  "Return true for a bounded proper list whose elements satisfy PREDICATE."
  (and (lsp-configuration--proper-list-p value)
       (<= (length value) *lsp-configuration-maximum-list-elements*)
       (every predicate value)))

(-> lsp-configuration--plist (t list pathname &key (:server-name t)) list)
(defun lsp-configuration--plist (form allowed pathname &key server-name)
  "Validate unique allowed keys in an even proper property list."
  (unless (and (lsp-configuration--proper-list-p form) (evenp (length form)))
    (lsp-configuration--error "LSP configuration requires an even proper property list."
                              :pathname pathname :server-name server-name))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for (key value) on form by #'cddr
          do (unless (member key allowed)
               (lsp-configuration--error (format nil "Unknown LSP configuration key ~S." key)
                                         :pathname pathname :server-name server-name :field key))
             (when (gethash key seen)
               (lsp-configuration--error (format nil "Duplicate LSP configuration key ~S." key)
                                         :pathname pathname :server-name server-name :field key))
             (setf (gethash key seen) t)))
  form)

(-> lsp-configuration--property (list keyword &key (:required-p boolean) (:pathname t) (:server-name t)) t)
(defun lsp-configuration--property (form key &key required-p pathname server-name)
  "Return a validated property or report a missing required field."
  (let* ((missing (gensym)) (value (getf form key missing)))
    (if (eq value missing)
        (when required-p
          (lsp-configuration--error (format nil "Missing required LSP key ~S." key)
                                    :pathname pathname :server-name server-name :field key))
        value)))

(-> lsp-configuration--read-source (pathname) string)
(defun lsp-configuration--read-source (pathname)
  "Read a byte-bounded UTF-8 regular configuration file."
  (handler-case
      (let ((stream (platform-open-regular-file *platform* pathname :follow-links-p t))
            (buffer (make-array (1+ *lsp-configuration-maximum-bytes*)
                                :element-type '(unsigned-byte 8))))
        (unwind-protect
             (let ((count (read-sequence buffer stream)))
               (when (> count *lsp-configuration-maximum-bytes*)
                 (lsp-configuration--error "The native LSP configuration exceeds its byte bound."
                                           :pathname pathname))
               (sb-ext:octets-to-string buffer :external-format ':utf-8 :end count))
          (close stream)))
    (lsp-configuration-error (condition) (error condition))
    (serious-condition (cause)
      (lsp-configuration--error (format nil "Could not read native LSP configuration: ~A" cause)
                                :pathname pathname :cause cause))))

(-> lsp-configuration--read-form (pathname) list)
(defun lsp-configuration--read-form (pathname)
  "Parse one declarative form with a closed atom grammar and no reader evaluation."
  (handler-case
      (read-source (lsp-configuration--read-source pathname)
                   (make-source-grammar
                    :label "Native LSP configuration"
                    :keywords '(:version :servers :name :command :arguments :extensions
                                :language-id :root-markers :initialization-options :settings
                                :timeout-seconds :disabled-p)
                    :maximum-depth 32 :maximum-nodes 16384
                    :improper-lists-permitted-p nil
                    :allowed-atom-predicate
                    (lambda (value)
                      (or (null value) (eq value t) (stringp value) (integerp value) (keywordp value)))))
    (sexp-config-error (cause)
      (lsp-configuration--error (sexp-config-error-message cause)
                                :pathname pathname :cause cause))))

(-> lsp-configuration--json-object-p (t) boolean)
(defun lsp-configuration--json-object-p (value)
  "Return true when VALUE is valid JSON whose top-level value is an object."
  (and (lsp-configuration--bounded-string-p value)
       (json-object-source-p value)))

(-> lsp-configuration--server (list pathname) lsp-server-configuration)
(defun lsp-configuration--server (form pathname)
  "Validate and construct one server definition with decoded JSON options."
  (lsp-configuration--plist form
                            '(:name :command :arguments :extensions :language-id
                              :root-markers :initialization-options :settings
                              :timeout-seconds :disabled-p)
                            pathname)
  (let* ((name (lsp-configuration--property form :name :required-p t :pathname pathname))
         (command (lsp-configuration--property form :command :required-p t :pathname pathname))
         (arguments (or (lsp-configuration--property form :arguments) nil))
         (extensions (or (lsp-configuration--property form :extensions) nil))
         (language-id (lsp-configuration--property form :language-id :required-p t :pathname pathname))
         (root-markers (or (lsp-configuration--property form :root-markers) nil))
         (initialization-options (lsp-configuration--property form :initialization-options))
         (settings (lsp-configuration--property form :settings))
         (timeout (if (member :timeout-seconds form) (lsp-configuration--property form :timeout-seconds) 30))
         (disabled-p (or (lsp-configuration--property form :disabled-p) nil)))
    (labels ((string-field (value field &key empty-p)
               (unless (lsp-configuration--bounded-string-p value :empty-p empty-p)
                 (lsp-configuration--error (format nil "LSP ~S must be a bounded string." field)
                                           :pathname pathname :server-name name :field field)))
             (string-list (value field &key empty-p)
               (unless (lsp-configuration--list-p value
                                                   (lambda (item)
                                                     (lsp-configuration--bounded-string-p item :empty-p empty-p)))
                 (lsp-configuration--error (format nil "LSP ~S must be a bounded list of strings." field)
                                           :pathname pathname :server-name name :field field))))
      (string-field name :name)
      (string-field command :command)
      (string-field language-id :language-id)
      (string-list arguments :arguments :empty-p t)
      (string-list extensions :extensions)
      (string-list root-markers :root-markers)
      (unless (and extensions
                   (every (lambda (marker)
                            (and (not (member marker '("." "..") :test #'string=))
                                 (not (find-if (lambda (character) (find character "/\\:*?[]")) marker))))
                          root-markers))
        (lsp-configuration--error "LSP requires nonempty extensions and literal root marker filenames."
                                  :pathname pathname :server-name name))
      (when initialization-options
        (unless (lsp-configuration--json-object-p initialization-options)
          (lsp-configuration--error "LSP :INITIALIZATION-OPTIONS must be a JSON object string."
                                    :pathname pathname :server-name name :field :initialization-options)))
      (when settings
        (unless (lsp-configuration--json-object-p settings)
          (lsp-configuration--error "LSP :SETTINGS must be a JSON object string."
                                    :pathname pathname :server-name name :field :settings)))
      (unless (and (integerp timeout) (<= 1 timeout *lsp-configuration-maximum-timeout-seconds*))
        (lsp-configuration--error "LSP :TIMEOUT-SECONDS must be an integer from 1 through 120."
                                  :pathname pathname :server-name name :field :timeout-seconds))
      (unless (or (null disabled-p) (eq disabled-p t))
        (lsp-configuration--error "LSP :DISABLED-P must be exactly T or NIL."
                                  :pathname pathname :server-name name :field :disabled-p))
      (make-instance 'lsp-server-configuration :name (copy-seq name) :command (copy-seq command)
                     :arguments (mapcar #'copy-seq arguments) :extensions (mapcar #'copy-seq extensions)
                     :language-id (copy-seq language-id) :root-markers (mapcar #'copy-seq root-markers)
                     :initialization-options (and initialization-options (json-decode initialization-options))
                     :settings (if settings (json-decode settings) (json-object)) :timeout-seconds timeout
                     :disabled-p (and disabled-p t)))))

(-> lsp-load-configurations (configuration) list)
(defun lsp-load-configurations (configuration)
  "Read CONFIGURATION's user-owned lsp.sexp, or return NIL when absent."
  (let ((pathname (lsp-configuration-path configuration)))
    (unless (probe-file pathname)
      (return-from lsp-load-configurations nil))
    (let ((form (lsp-configuration--read-form pathname)))
      (lsp-configuration--plist form '(:version :servers) pathname)
      (unless (eql (lsp-configuration--property form :version :required-p t :pathname pathname)
                   *lsp-configuration-version*)
        (lsp-configuration--error "LSP configuration must use version 1." :pathname pathname :field :version))
      (let ((servers (lsp-configuration--property form :servers :required-p t :pathname pathname)))
        (unless (and (lsp-configuration--proper-list-p servers)
                     (<= (length servers) *lsp-configuration-maximum-servers*))
          (lsp-configuration--error "LSP :SERVERS must be a list of at most 32 entries."
                                    :pathname pathname :field :servers))
        (let ((result (mapcar (lambda (server) (lsp-configuration--server server pathname)) servers))
              (seen (make-hash-table :test #'equal)))
          (dolist (server result)
            (let ((name (lsp-server-configuration-name server)))
              (when (gethash name seen)
                (lsp-configuration--error "Duplicate LSP server name."
                                          :pathname pathname :server-name name :field :name))
              (setf (gethash name seen) t)))
          result)))))

(-> lsp-project-root (pathname pathname list) pathname)
(defun lsp-project-root (path workspace markers)
  "Return the nearest ancestor of PATH containing one of MARKERS.
Only directories within WORKSPACE are considered; WORKSPACE is the fallback."
  (let* ((workspace (uiop:ensure-directory-pathname
                     (platform-truename *platform* workspace)))
         (path (platform-truename *platform* (pathname path)))
         (directory (if (uiop:directory-pathname-p path)
                        (uiop:ensure-directory-pathname path)
                        (uiop:pathname-directory-pathname path))))
    (unless (uiop:string-prefix-p (namestring workspace) (namestring directory))
      (return-from lsp-project-root workspace))
    (loop repeat *workspace-project-depth-limit*
          for candidate = directory then (uiop:pathname-parent-directory-pathname candidate)
          for parent = (uiop:pathname-parent-directory-pathname candidate)
          when (some (lambda (marker)
                       (or (uiop:file-exists-p (merge-pathnames marker candidate))
                           (uiop:directory-exists-p (merge-pathnames marker candidate))))
                     markers)
            return candidate
          when (equal candidate workspace)
            return workspace
          when (equal candidate parent)
            return workspace
          finally (return workspace))))
