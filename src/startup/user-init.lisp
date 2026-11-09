(in-package #:autolith)

;;;; -- User Initialization --

(defvar *user-init-configuration* nil
  "The configuration whose executable initialization files are loading.")

(defvar *user-init-pathname* nil
  "The executable initialization pathname currently being loaded.")

(defvar *user-init-layer* nil
  "The site, directory, or user initialization layer currently being loaded.")

(-> user-init--registry-lifecycle (symbol symbol symbol) registry-lifecycle)
(defun user-init--registry-lifecycle
    (snapshot-function restore-function remove-source-function)
  "Adapt named registry operations, resolving their current definitions on each call."
  (make-registry-lifecycle
   :snapshot-function      (lambda () (funcall snapshot-function))
   :restore-function       (lambda (snapshot) (funcall restore-function snapshot))
   :remove-source-function (lambda (source) (funcall remove-source-function source))))

(defparameter *extension-registry-lifecycles*
  (list
   (user-init--registry-lifecycle
    'context--registry-snapshot
    'context--registry-restore
    'context--remove-registration-source)
   (user-init--registry-lifecycle
    'application-command--registry-snapshot
    'application-command--registry-restore
    'application-command--remove-registration-source)
   (user-init--registry-lifecycle
    'mcp--registry-snapshot
    'mcp--registry-restore
    'mcp--remove-registration-source)
   (user-init--registry-lifecycle
    'provider--registry-snapshot
    'provider--registry-restore
    'provider--remove-registration-source))
  "Ordered participants in executable initialization's registration transaction.
Each participant retains its own locks, validation, and derived-state publication.")

(-> user-init--present-entry (pathname keyword string) list)
(defun user-init--present-entry (pathname layer description)
  "Return one LAYER entry when PATHNAME is a present regular file."
  (handler-case
      (when (mcp-configuration--source-present-p
             pathname :description description)
        (list (list layer pathname)))
    (mcp-configuration-error (cause)
      (let ((pathname
              (or (mcp-configuration-error-pathname cause)
                  pathname)))
        (error 'user-init-error
               :message
               (format nil
                       "Could not inspect ~A executable initialization at ~A: ~A"
                       (string-downcase (symbol-name layer)) pathname cause)
               :pathname pathname
               :layer    layer
               :cause    cause)))))

(-> user-init--directory-entries (configuration) list)
(defun user-init--directory-entries (configuration)
  "Return trusted directory initialization entries in inheritance order."
  (handler-case
      (mapcar (lambda (pathname)
                (list ':directory pathname))
              (directory-configuration-active-init-paths configuration))
    (mcp-configuration-error (cause)
      (let ((pathname
              (or (mcp-configuration-error-pathname cause)
                  (configuration-directory-scopes-path configuration))))
        (error 'user-init-error
               :message
               (format nil
                       "Could not inspect directory executable initialization at ~A: ~A"
                       pathname cause)
               :pathname pathname
               :layer    ':directory
               :cause    cause)))))

(-> user-init--entries (configuration) list)
(defun user-init--entries (configuration)
  "Return site, trusted directory, and user initialization entries in load order."
  (let ((site (configuration-site-init-path configuration))
        (user (configuration-user-init-path configuration)))
    (append
     (when site
       (user-init--present-entry site ':site "site initialization file"))
     (user-init--directory-entries configuration)
     (user-init--present-entry user ':user "user initialization file"))))

(-> user-init-load (configuration) (option pathname))
(defun user-init-load (configuration)
  "Load CONFIGURATION's executable initialization files and return the last path.

The optional site file loads first. Trusted directory files then load from
outermost to nearest, followed by the global user file. They are read in the
AUTOLITH package after tracked and privately committed definitions have loaded.
They execute with the user's full privileges. Registration changes roll back
after failure, but arbitrary Lisp side effects do not generally have reversible
semantics."
  (with-extension-registry-transaction
    (let* ((entries         (user-init--entries configuration))
           (registries      (copy-list *extension-registry-lifecycles*))
           (snapshots       (mapcar #'registry-snapshot registries))
           (loaded-pathname nil)
           (current-entry   nil))
      (handler-case
          (progn
            (dolist (source '(:site :user))
              (dolist (registry registries)
                (registry-remove-source registry source)))
            (dolist (entry entries loaded-pathname)
              (setf current-entry entry)
              (let* ((layer                     (first entry))
                     (pathname                  (second entry))
                     (*package*                 (find-package '#:autolith))
                     (*user-init-loading-p*     t)
                     (*user-init-configuration* configuration)
                     (*configuration*           configuration)
                     (*user-init-pathname*      pathname)
                     (*user-init-layer*         layer)
                     (*extension-registration-source*
                       (if (eq layer ':site) ':site ':user)))
                (load pathname :verbose nil :print nil)
                (setf loaded-pathname pathname))))
        (serious-condition (cause)
          (mapc #'registry-restore registries snapshots)
          (let* ((entry    (or current-entry
                               (first entries)
                               (list ':user
                                     (configuration-user-init-path configuration))))
                 (layer    (first entry))
                 (pathname (second entry)))
            (error 'user-init-error
                   :message
                   (format nil
                           "Could not load ~A executable initialization at ~A: ~A"
                           (string-downcase (symbol-name layer)) pathname cause)
                   :pathname pathname
                   :layer    layer
                   :cause    cause)))))))
