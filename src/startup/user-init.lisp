(in-package #:autolith)

;;;; -- User Initialization --

(defvar *user-init-configuration* nil
  "The configuration whose executable initialization files are loading.")

(defvar *user-init-pathname* nil
  "The executable initialization pathname currently being loaded.")

(defvar *user-init-layer* nil
  "The site, directory, or user initialization layer currently being loaded.")


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
               :layer layer
               :cause cause)))))

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
               :layer ':directory
               :cause cause)))))

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
    (let ((entries (user-init--entries configuration))
          (context-registrations (context--registry-snapshot))
          (command-registrations (application-command--registry-snapshot))
          (mcp-registrations (mcp--registry-snapshot))
          (provider-registrations (provider--registry-snapshot))
          (loaded-pathname nil)
          (current-entry nil))
      (handler-case
          (progn
            (dolist (source '(:site :user))
              (context--remove-registration-source source)
              (application-command--remove-registration-source source)
              (mcp--remove-registration-source source)
              (provider--remove-registration-source source))
            (dolist (entry entries loaded-pathname)
              (setf current-entry entry)
              (let* ((layer (first entry))
                     (pathname (second entry))
                     (*package* (find-package '#:autolith))
                     (*user-init-loading-p* t)
                     (*user-init-configuration* configuration)
                     (*user-init-pathname* pathname)
                     (*user-init-layer* layer)
                     (*extension-registration-source*
                       (if (eq layer ':site) ':site ':user)))
                (load pathname :verbose nil :print nil)
                (setf loaded-pathname pathname))))
        (serious-condition (cause)
          (context--registry-restore context-registrations)
          (application-command--registry-restore command-registrations)
          (mcp--registry-restore mcp-registrations)
          (provider--registry-restore provider-registrations)
          (let* ((entry (or current-entry
                            (first entries)
                            (list ':user
                                  (configuration-user-init-path configuration))))
                 (layer (first entry))
                 (pathname (second entry)))
            (error 'user-init-error
                   :message
                   (format nil
                           "Could not load ~A executable initialization at ~A: ~A"
                           (string-downcase (symbol-name layer)) pathname cause)
                   :pathname pathname
                   :layer layer
                   :cause cause)))))))
