(in-package #:autolith)

;;;; -- User Initialization --

(defvar *user-init-configuration* nil
  "The configuration whose executable initialization files are loading.")

(defvar *user-init-pathname* nil
  "The executable initialization pathname currently being loaded.")


(-> user-init--pathnames (configuration) list)
(defun user-init--pathnames (configuration)
  "Return trusted directory and global initialization files in load order."
  (handler-case
      (let ((global (configuration-user-init-path configuration)))
        (append
         (directory-configuration-active-init-paths configuration)
         (when (mcp-configuration--source-present-p
                global :description "user initialization file")
           (list global))))
    (mcp-configuration-error (cause)
      (let ((pathname
              (or (mcp-configuration-error-pathname cause)
                  (configuration-user-init-path configuration))))
        (error 'user-init-error
               :message
               (format nil "Could not inspect executable initialization at ~A: ~A"
                       pathname cause)
               :pathname pathname
               :cause cause)))))

(-> user-init-load (configuration) (option pathname))
(defun user-init-load (configuration)
  "Load CONFIGURATION's executable initialization files and return the last path.

Trusted directory files load from outermost to nearest, followed by the global
user file. They are read in the AUTOLITH package after tracked and privately
committed definitions have loaded. They execute with the user's full privileges.
Registration changes roll back after failure, but arbitrary Lisp side effects do
not generally have reversible semantics."
  (with-extension-registry-transaction
    (let ((pathnames (user-init--pathnames configuration))
          (context-registrations (context--registry-snapshot))
          (command-registrations (application-command--registry-snapshot))
          (mcp-registrations (mcp--registry-snapshot))
          (provider-registrations (provider--registry-snapshot))
          (loaded-pathname nil)
          (current-pathname nil))
      (handler-case
          (progn
            (context--remove-registration-source ':user)
            (application-command--remove-registration-source ':user)
            (mcp--remove-registration-source ':user)
            (provider--remove-registration-source ':user)
            (dolist (pathname pathnames loaded-pathname)
              (setf current-pathname pathname)
              (let ((*package* (find-package '#:autolith))
                    (*user-init-loading-p* t)
                    (*user-init-configuration* configuration)
                    (*configuration* configuration)
                    (*user-init-pathname* pathname))
                (load pathname :verbose nil :print nil)
                (setf loaded-pathname pathname))))
        (serious-condition (cause)
          (context--registry-restore context-registrations)
          (application-command--registry-restore command-registrations)
          (mcp--registry-restore mcp-registrations)
          (provider--registry-restore provider-registrations)
          (let ((pathname (or current-pathname
                              (first pathnames)
                              (configuration-user-init-path configuration))))
            (error 'user-init-error
                   :message
                   (format nil "Could not load executable initialization at ~A: ~A"
                           pathname cause)
                   :pathname pathname
                   :cause cause)))))))
