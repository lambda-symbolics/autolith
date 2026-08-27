(in-package #:autolith)

;;;; -- Directory-Scoped MCP Configuration --

(defparameter *directory-configuration-version* 1
  "The directory-scope trust manifest version accepted by Autolith.")

(defparameter *directory-configuration-maximum-directories* 64
  "The maximum number of trusted directory anchors in one manifest.")

(defparameter *directory-configuration-path-maximum-characters* 4096
  "The maximum character length of one trusted directory pathname.")


(-> configuration-directory-mcp-path (pathname) pathname)
(defun configuration-directory-mcp-path (directory)
  "Return the inherited native MCP pathname beneath trusted DIRECTORY."
  (merge-pathnames ".autolith/mcp.sexp"
                   (uiop:ensure-directory-pathname directory)))

(-> configuration-directory-init-path (pathname) pathname)
(defun configuration-directory-init-path (directory)
  "Return the inherited executable initialization path beneath trusted DIRECTORY."
  (merge-pathnames ".autolith/init.lisp"
                   (uiop:ensure-directory-pathname directory)))

(-> directory-configuration--canonical-directory
    (string pathname)
    pathname)
(defun directory-configuration--canonical-directory (value manifest-pathname)
  "Return trusted directory VALUE as an existing canonical directory."
  (unless
      (mcp-configuration--bounded-string-p
       value *directory-configuration-path-maximum-characters*)
    (mcp-configuration--error
     "A trusted directory must be a bounded non-empty pathname string."
     :pathname manifest-pathname
     :field ':directories))
  (let ((expanded (configuration--expanded-working-directory value)))
    (unless (uiop:absolute-pathname-p expanded)
      (mcp-configuration--error
       (format nil "Trusted directory ~S must be absolute or begin with ~~/."
               value)
       :pathname manifest-pathname
       :field ':directories))
    (handler-case
        (let ((directory
                (uiop:directory-exists-p
                 (uiop:ensure-pathname expanded
                                       :ensure-directory t
                                       :want-non-wild t))))
          (unless directory
            (mcp-configuration--error
             (format nil "Trusted directory ~S does not exist." value)
             :pathname manifest-pathname
             :field ':directories))
          (uiop:ensure-directory-pathname (truename directory)))
      (mcp-configuration-error (condition)
        (error condition))
      (serious-condition (cause)
        (mcp-configuration--error
         (format nil "Could not resolve trusted directory ~S: ~A"
                 value cause)
         :pathname manifest-pathname
         :field ':directories
         :cause cause)))))

(-> directory-configuration-read-trusted-directories (configuration) list)
(defun directory-configuration-read-trusted-directories (configuration)
  "Read canonical trusted directory anchors from CONFIGURATION's manifest."
  (let ((pathname (configuration-directory-scopes-path configuration)))
    (unless (mcp-configuration--source-present-p pathname)
      (return-from directory-configuration-read-trusted-directories nil))
    (let ((form (mcp-configuration--read-form pathname)))
      (mcp-configuration--validate-plist
       form '(:version :directories) :pathname pathname)
      (unless (eql
               (mcp-configuration--property
                form :version :required-p t :pathname pathname)
               *directory-configuration-version*)
        (mcp-configuration--error
         (format nil "Directory scopes must use version ~D."
                 *directory-configuration-version*)
         :pathname pathname
         :field ':version))
      (let ((directories
              (mcp-configuration--property
               form :directories :required-p t :pathname pathname)))
        (unless (mcp-configuration--proper-list-p directories)
          (mcp-configuration--error
           "Directory-scope :DIRECTORIES must be a proper list."
           :pathname pathname
           :field ':directories))
        (when (> (length directories)
                 *directory-configuration-maximum-directories*)
          (mcp-configuration--error
           (format nil "Directory-scope :DIRECTORIES exceeds the limit of ~D."
                   *directory-configuration-maximum-directories*)
           :pathname pathname
           :field ':directories))
        (let ((result nil)
              (seen (make-hash-table :test #'equal)))
          (dolist (value directories)
            (unless (stringp value)
              (mcp-configuration--error
               "Every trusted directory must be a pathname string."
               :pathname pathname
               :field ':directories))
            (let* ((directory
                     (directory-configuration--canonical-directory value pathname))
                   (key (namestring directory)))
              (when (gethash key seen)
                (mcp-configuration--error
                 (format nil "Duplicate trusted directory ~S." value)
                 :pathname pathname
                 :field ':directories))
              (setf (gethash key seen) t)
              (setf result (append result (list directory)))))
          result)))))

(-> directory-configuration-active-directories (configuration) list)
(defun directory-configuration-active-directories (configuration)
  "Return trusted anchors containing CONFIGURATION's current workspace.

Anchors are ordered from the outermost directory to the nearest directory."
  (let* ((workspace
           (uiop:ensure-directory-pathname
            (truename (configuration-working-directory configuration))))
         (active
           (remove-if-not
            (lambda (directory)
              (not (null (uiop:subpathp workspace directory))))
            (directory-configuration-read-trusted-directories configuration))))
    (stable-sort active #'< :key (lambda (directory)
                                   (length (namestring directory))))))

(-> directory-configuration-active-mcp-paths (configuration) list)
(defun directory-configuration-active-mcp-paths (configuration)
  "Return existing inherited MCP files for CONFIGURATION in precedence order."
  (loop for directory in (directory-configuration-active-directories configuration)
        for pathname = (configuration-directory-mcp-path directory)
        when (mcp-configuration--source-present-p pathname)
          collect pathname))

(-> directory-configuration-active-init-paths (configuration) list)
(defun directory-configuration-active-init-paths (configuration)
  "Return existing inherited Lisp files for CONFIGURATION in load order."
  (loop for directory in (directory-configuration-active-directories configuration)
        for pathname = (configuration-directory-init-path directory)
        when (mcp-configuration--source-present-p
              pathname :description "trusted directory initialization file")
          collect pathname))

(-> directory-configuration--merge-definitions (list list) list)
(defun directory-configuration--merge-definitions (definitions additions)
  "Merge ADDITIONS into DEFINITIONS by case-sensitive server name."
  (dolist (addition additions definitions)
    (let* ((name (mcp-server-configuration-name addition))
           (position
             (position name definitions
                       :test #'string=
                       :key #'mcp-server-configuration-name)))
      (if position
          (setf (nth position definitions) addition)
          (setf definitions (append definitions (list addition)))))))

(-> directory-configuration-read-mcp (configuration) list)
(defun directory-configuration-read-mcp (configuration)
  "Read and merge CONFIGURATION's active inherited MCP definitions."
  (let ((definitions nil))
    (dolist (pathname (directory-configuration-active-mcp-paths configuration)
             definitions)
      (setf definitions
            (directory-configuration--merge-definitions
             definitions
             (mcp-configuration-read-path pathname))))))

(-> mcp-configuration--registrations (list keyword) list)
(defun mcp-configuration--registrations (definitions source)
  "Return registration objects for DEFINITIONS attributed to SOURCE."
  (mapcar
   (lambda (definition)
     (make-instance 'mcp-server-registration
                    :configuration definition
                    :source source))
   definitions))

(-> mcp-configuration-load (configuration) list)
(defun mcp-configuration-load (configuration)
  "Atomically replace site, global, and inherited native MCP registrations."
  (with-extension-registry-transaction
    (let ((site-definitions
            (let ((pathname (configuration-site-mcp-path configuration)))
              (when pathname
                (mcp-configuration-read-path pathname))))
          (global-definitions (mcp-configuration-read configuration))
          (directory-definitions (directory-configuration-read-mcp configuration)))
      (with-lock-held (*mcp-server-registry-lock*)
        (let ((candidate
                (remove-if
                 (lambda (registration)
                   (member
                    (mcp-server-registration-source registration)
                    '(:site-config :config :directory)))
                 *mcp-server-registrations*)))
          (setf candidate
                (append
                 candidate
                 (mcp-configuration--registrations
                  site-definitions ':site-config)
                 (mcp-configuration--registrations
                  global-definitions ':config)
                 (mcp-configuration--registrations
                  directory-definitions ':directory)))
          (mcp--validate-registration-list candidate)
          (setf *mcp-server-registrations* candidate))))
    (mcp-server-registrations)))
